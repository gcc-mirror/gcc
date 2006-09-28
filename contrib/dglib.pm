# Library of functions for dealing with DejaGNU, or which are otherwise
# generally useful for the DejaGNU tool stack.
#
# Author: Matthew Sachs <msachs@apple.com>
#
# Functions:
#	parseLogFile: See "sub parseLogFile" below for details.  This function
#		returns a detailed parse of a DejaGNU log or sum file.
#	ispass: Takes a DejaGNU result (e.g. "PASS", "XPASS") and returns
#		true if and only if it is a passing result (PASS, XFAIL, or
#		KFAIL.)
#
# Copyright (c) 2006 Free Software Foundation.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

package dglib;

use strict;
use warnings;
use Exporter;

our @ISA = qw(Exporter);
our @EXPORT = qw(ispass parseLogFile);

use File::Basename;
use POSIX qw(mktime);


# Create a group hierarchy, returning the leaf node
sub mkGroupPath {
	my($root, $groups, @newgroups) = @_;

	my $parent = $root;
	my $fullname = "";
	foreach my $group(@newgroups) {
		$fullname .= "/" if $fullname;
		$fullname .= $group;
		if(exists($groups->{$fullname})) {
			$parent = $groups->{$fullname};
		} else {
			my $newgroup = {name => $group, parent => $parent};
			$groups->{$fullname} = $newgroup;
			$parent->{testgroup} ||= [];
			push @{$parent->{testgroup}}, $newgroup;
			$parent = $newgroup;
		}
	}

	return $parent;
}

# Extract information from DejaGNU log or sum files.
# Options, if provided, should be a hashref with zero or more of the following keys:
#	gccdir:
# 		Passing in the full path to the root of the gcc/testsuite directory
#		will help in the parsing, but if it isn't provided, it will be guessed.
#	diagnostics:
#		If set to 0, diagnostics will not be returned.  This can save a lot
#		of memory if you are not using this information.
#	fullname:
#		If set to 0, the fullname key will not be included in tests.
# Returns a hash with the following keys:
#	incomplete: 1 if the summary file appears truncated, otherwise 0
#	diagnostics: List of (type, value) for any errors detected.  Type can be ERROR, WARNING, or NOTE.
#	test: Array of root-level tests, with keys:
#		name: Name of the test, relative to the enclosing test group.
#		fullname: Fully-qualified name of the test.
#		result: DejaGNU result (PASS, FAIL, XPASS, &c)
#		detail: For multi-phase (e.g. compile/link/execute), this will be
#		        the furthest phase which the test was able to attempt,
#			so if the result is FAIL and this is "link phase", the test
#			compiled but failed to link.  This key may contain other
#			auxiliary data.
#		pseudotest: If 1, this test may not really exist; see "pseudotest" below.
#	testgroup: Array of root-level testgroups, with keys:
#		name: Name of the group.
#		parent: Parent test group.
#		test: As per above.
#		testgroup: Child test groups.
#	compiler: Version string from compiler used to run the tests (if detected)
sub parseLogFile($;$) {
	my($logfile, $options) = @_;
	$options ||= {};
	my $gccdir = $options->{gccdir} || "";
	my $return_diags = exists($options->{diagnostics}) ? $options->{diagnostics} : 1;
	my $emit_fullname = exists($options->{fullname}) ? $options->{fullname} : 1;
	my $is_gdb = 0;
	my $gdbhack = "";

	my %ret = (incomplete => 1, diagnostics => [], testgroup => []);
	my(%testindex, %groupindex);

	open(LOGFILE, $logfile) or die "Couldn't open log file $logfile: $!\n";

	my($currgroup, $currtest, $lastrun);
	$currgroup = \%ret;

	my %monmap = (Jan => 0, Feb => 1, Mar => 2, Apr => 3, May => 4, Jun => 5, Jul => 6, Aug => 7, Sep => 8, Oct => 9, Nov => 10, Dec => 11);

	# We don't want gccdir matching on an empty string.
	$gccdir ||= "this will never match, or my name isn't Reginald St. Croix";

	my $line = 1;
	while(<LOGFILE>) {
		chomp;
		s/\x{d}$//; #^M
		next if $_ eq "";

		if(/^gcc version/) {
			$ret{compiler} = $_;
		} elsif(/^got a .* signal, interrupted by user /) {
			$ret{incomplete} = 2;
		} elsif(/^\s*=== gdb/) {
			$is_gdb = 1;
			# The log file from the GDB test suite is prone to have random crap
			# in front of test result lines, so we need to be looser about how
			# we parse those for GDB.
			$gdbhack = ".*";
		} elsif(/^(Test Run By \S+ on|runtest completed at) ... (.{3}) (\d{1,2}) (\d{1,2}):(\d{1,2}):(\d{1,2}) (\d{4})/) {
			my $time = mktime($6, $5, $4, $3, $monmap{$2}, $7 - 1900);
			if($1 eq "runtest completed at") {
				$ret{end_time} = $time;
			} else {
				$ret{start_time} = $time;
			}
		} elsif(m<^Running (?!target )\Q$gccdir\E/?(\S+)> or m<^Running (?!target )\S*?((?:gcc|gdb|libstdc\+\+-v3)/testsuite/\S+)>) {
			# We keep track of the last "Running foo/bar/baz.exp" line because
			# some tests don't bother printing out the full paths of their files,
			# and this gives us the directory information.

			$lastrun = $1;
			$lastrun =~ s!/[^/]*/\.\.!!; # foo/bar/../baz -> foo/baz
			$currgroup = mkGroupPath(\%ret, \%groupindex, split(m!/!, $lastrun));
			#$currgroup->{testfile} = $lastrun;
		} elsif(/^Executing on (.*?):(.*)/) {
			# Okay, if it's "Executing on host", it's a new
			# file.  If it's "Executing on unix", it's probably
			# a test within the file -- an execution test, specifically --
			# (makes sense, no?)  But not always, sometimes we
			# see "Executing on unix" outside the context of a
			# file.

			# Try to pick out the gccdir-relative filename.
			# If we can't find it, it isn't really a new testfile,
			# but a derived file.
			my($exectype, $execwhat) = ($1, $2);
			next if $execwhat =~ /^dsymutil/;
			$execwhat =~
				s!.*?\s\Q$gccdir\E/?(\S+).*!$1! or
				s!.*?/((?:gcc|gdb|libstdc\+\+-v3)/testsuite/\S+).*!$1! or
				$exectype = "unix";

			if($exectype eq "host" or !$currgroup) {
				# New file

				my $nogroup = 0;
				if($execwhat =~ / /) {
					# We probably haven't parsed the file correctly.
					# Try getting it from $lastrun.

					$execwhat = dirname($lastrun) . "/" . basename($execwhat) if $lastrun and $execwhat;
					$execwhat =~ s/\s.*//;

					# At the end of each tool, it invokes "gcc -v" or "c++ -v"
					# as a test.  We don't really want to treat this as a test.
					if($execwhat =~ m!/(gcc|c\+\+)$!) {
						undef $currtest;
						undef $currgroup;
						$nogroup = 1;
					}
				}

				if(!$nogroup) {
					undef $currtest;
					$execwhat =~ s!/[^/]*/\.\.!!; # foo/bar/../baz -> foo/baz

					if($lastrun) {
						my $lastbase = dirname($lastrun);
						my $basegroup = $execwhat;
						$basegroup =~ s!^\Q$lastbase\E/!!;
						$execwhat = "$lastrun/$basegroup";
					}

					$currgroup = mkGroupPath(\%ret, \%groupindex, split(m!/!, $execwhat));
					#$currgroup->{testfile} = $execwhat;
				}
			} else {
				# New test within current file

				$currtest = {};
			}
		} elsif(/^# of/) {
			# This line appears should appear near the end of summary files.
			# If it doesn't, something went wrong.

			if($ret{incomplete} == 2) {
				#Ah, but we previously saw indication that we were killed via a signal.
				$ret{incomplete} = 1;
			} else {
				$ret{incomplete} = 0;
			}
		} elsif(/^testcase .* completed/) {
			# End of a .exp file
			undef $currtest;
			undef $currgroup;
		} elsif(/^$gdbhack(FAIL|PASS|UNRESOLVED|UNSUPPORTED|UNTESTED|XFAIL|XPASS|KFAIL|KPASS): (.*)/) {
			# If the currtest already has a name, that means we've already seen
			# its results, so what we have now is a new test.  However, if we
			# haven't seen results for currtest yet, that means currtest just
			# has some diagnostics associated with it but no actual results,
			# so just use that one.
			undef $currtest if $currtest->{name};

			my $phase = ""; # compile/link/execute
			my($test, $result) = ($2, $1);

			# Compile/(link/)execute combining
			if($test =~ /^(.*) compile\s*$/) {
				$test = "$1 compile,link,execute";
				$phase = "compile";
			} elsif($test =~ /^(.*)-(.*) (link|execute)\s*$/) {
				$test = "$1 compile,link,execute";
				if($3 eq "link") {
					$phase = "link";
				} else {
					$phase = "execute";
				}
			} elsif($test =~ /(compile|compilation|execute|execution)/) {
				my $phasematch = $1;
				if($test =~ /^com/) {
					$phase = "compile";
				} else {
					$phase = "execute";
				}
				$test =~ s!\Q$phasematch\E!compile,execute!;
			}

			# gcov tests behave in non-standard fashion.
			my $failwhy = "";
			$test =~ s/ gcov failed: (.*)// and $failwhy = $1;

			# And some other tests have random information after a colon :(
			# But for scan-assembler, this really is part of the name.
			if(!$is_gdb and $test !~ /scan-assembler/ and $test =~ s/:\s*(.+)//) {
				$failwhy = $1;
			}

			$test =~ s/\s*$//;
			$test =~ s/^\s*$//;

			# Sometimes there's a test which shows up as:
			#	foo (test for excess errors)
			#	foo (something else)
			#	foo: error executing dg-final
			# if it runs, but just:
			#	foo
			# if it doesn't.  When we see the top form, we create a
			# "pseudotest" in the bottom form, so that comparisons
			# can be made.
			my $basetest = $test;
			$basetest =~ s/:? .*//;

			if(exists($testindex{$test}) and !$testindex{$test}->{pseudotest}) {
				$currtest = $testindex{$test};
				if(ispass($currtest->{result})) {
					$currtest->{result} = $result;
					$currtest->{detail} = "$phase phase";
					$currtest->{detail} .= "; $failwhy" if $failwhy;
				}
			} else {
				# This might have been created earlier as a pseudotest.
				# If so, overwrite it.
				$currtest ||= $testindex{$test} || {};

				$currtest->{name} = basename($test);
				if($emit_fullname) {
					$currtest->{fullname} = ($currgroup->{name} || dirname($test)) . "/$currtest->{name}";
				}
				my $grpname = $currgroup->{name} || "";
				$currtest->{name} =~ s/^\s*\Q$grpname\E\s*//;
				$currtest->{name} =~ s/^: // if $is_gdb;
				# Sometimes there's a test at the root of the group.
				# For instance, you'll have:
				#	FAIL: foo/bar.c (test for excess errors)
				#	UNRESOLVED: foo/bar.c: couldn't open "bar.s": no such file or directory
				# In this case, groupname *is* the entire name, so the regex above will make the test name empty.
				# In this case, we actually want to use the parent group and make this a test within that group.
				my $orig_currgroup = $currgroup;
				if(!$currtest->{name}) {
					$currtest->{name} = $grpname;
					$currgroup = $currgroup->{parent};
					$grpname = $currgroup->{name} || "";
				}

				$currtest->{result} = $result;
				if($phase and $failwhy) {
					$currtest->{detail} = "$phase phase; $failwhy" if $phase;
				} elsif($phase) {
					$currtest->{detail} = "$phase phase";
				} elsif($failwhy) {
					$currtest->{detail} = $failwhy;
				}

				$currgroup->{test} ||= [];
				push @{$currgroup->{test}}, $currtest;
				$testindex{$test} = $currtest;
				$currgroup = $orig_currgroup;

				if($basetest ne $test) {
					if(!exists($testindex{$basetest}) ) {
						my $btbase = basename($basetest);
						$testindex{$basetest} = {
							name => $btbase,
							result => $result,
							pseudotest => 1,
							fullname => $btbase
						};
						if($emit_fullname) {
							$testindex{basetest}->{fullname} = ($currgroup->{name} || dirname($basetest)) . "/$btbase";
						}
						push @{$currgroup->{parent}->{test}}, $testindex{$basetest};
					} else {
						# Only let the base test pass if all the sub-tests pass
						$testindex{$basetest}->{result} = $result if !ispass($result);
					}
				}

			}
		} elsif(/^\s+=== .* Summary ===\s*$/) {
			undef $currgroup;
			undef $currtest;
		}

		my $severity;
		if(/^(ERROR|WARNING|NOTE): (.*)/) {
			$severity = $1;
			my $message = $2;

			if($message eq "program timed out.") {
				$currtest->{result} = "TIMEDOUT";
			} elsif(
				$message =~ /can't read "(HOSTCC|libiconv)": no such variable/ or
				$message =~ /no files matched glob pattern/ or
				$message =~ /error executing dg-final: .*: no such file/
			) {
				$severity = "NOTE";
			}
		} else {
			$severity = "logline";
		}

		if($return_diags) {
			my $dobj;
			if($currtest) {
			$currtest->{diagnostics} ||= [];
				$dobj = $currtest->{diagnostics};
			} elsif($currgroup) {
				$currgroup->{diagnostics} ||= [];
				$dobj = $currgroup->{diagnostics};
			} else {
				$dobj = $ret{diagnostics};
			}

			push @$dobj, {message => $_, severity => $severity, line => $line};
		}
	} continue {
		$line++;
	}
	close LOGFILE;

	return %ret;
}

# Split a test into testdivs
sub splitTest($$) {
	my($root, $test) = @_;

	$test->{fullname} =~ /^(\S+)\s*(.*)/;
	my($path, $descriptor) = ($1, $2);
	my @nodes = split(m!/!, $path);
	push @nodes, $descriptor if $descriptor;
	my $lastnode = pop @nodes;

	my $hash = $root;
	foreach (@nodes) {
		$hash->{testdiv} ||= {};
		$hash = $hash->{testdiv}->{$_} ||= {};
	}


	$hash->{test} ||= {};
	$hash->{test}->{$lastnode} = $test;
}


# ==== Comparison ====

sub ispass($) {
	my $result = shift;

	if($result eq "PASS" or $result eq "XFAIL" or $result eq "KFAIL") {
		return 1;
	} else {
		return 0;
	}
}

1;
