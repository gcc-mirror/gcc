#! /usr/bin/perl -w

# This does trivial (and I mean _trivial_) conversion of Texinfo
# markup to Perl POD format.  It's intended to be used to extract
# something suitable for a manpage from a Texinfo document.

$output = 0;
$ignore = 0;
$skipping = 0;
%sects = ();
$section = "";
@icstack = ();
@endwstack = ();
@skstack = ();
$shift = "";
%defs = ();

while($_ = shift)
{
    if (/^-D(.*)$/) {
	if ($1 ne "") {
	    $flag = $1;
	} else {
	    $flag = shift;
	}
	die "no flag specified for -D\n"
	    unless $flag ne "";
	die "flags may only contain letters, digits, hyphens, and underscores\n"
	    unless $flag =~ /^[a-zA-Z0-9_-]+$/;
	$defs{$flag} = "";
    } elsif (/^-/) {
	usage();
    } else {
	$in = $_, next unless defined $in;
	$out = $_, next unless defined $out;
	usage();
    }
}

if (defined $in) {
    open(STDIN, $in) or die "opening \"$in\": $!\n";
}
if (defined $out) {
    open(STDOUT, ">$out") or die "opening \"$out\": $!\n";
}

while(<STDIN>)
{
    chomp;
    /^\@c man begin ([A-Z]+)/ and $sect = $1, $output = 1, next;
    /^\@c man end/ and do {
	$_ = $section;

	s/\@(?:dfn|var|emph|cite)\{([^\}]*)\}/I<$1>/g;
	s/\@(?:code|kbd)\{([^\}]*)\}/C<$1>/g;
	s/\@(?:samp|strong|key)\{([^\}]*)\}/B<$1>/g;
	s/\@value\{([a-zA-Z0-9_-]+)\}/$defs{$1}/g;
	s/\@sc\{([^\}]*)\}/\U$1/g;
	s/\@file\{([^\}]*)\}/F<$1>/g;
	s/\@(?:url|email)\{([^\}]*)\}/E<lt>C<$1>E<rt>/g;
	s/\@xref\{(?:[^\}]*)\}[^.]*.//g;
	s/\s+\(\@p[a-z]?ref\{(?:[^\}]*)\}\)//g;
	s/\@copyright\{\}//g;
	s/\@noindent\s*//g;
	s/\@refill//g;
	s/\@\././g;

	# Turn B<blah I<blah> blah> into B<blah> I<blah> B<blah> to
	# match Texinfo semantics of @emph inside @samp.

	s/&LT;/</g;
	s/&GT;/>/g;
	1 while (s/B<([^<>]*)I<([^>]+)>/B<$1>I<$2>B</g);
	1 while (s/I<([^<>]*)B<([^>]+)>/I<$1>B<$2>I</g);
	s/[BI]<>//g;
	s/([BI])<(\s+)([^>]+)>/$2$1<$3>/g;
	s/([BI])<([^>]+?)(\s+)>/$1<$2>$3/g;

	s/&lt;/E<lt>/g;
	s/&gt;/E<gt>/g;
	s/&lbrace;/\{/g;
	s/&rbrace;/\}/g;

	$sects{$sect} = $_;
	$section = "";
	$output = 0;
	next;
    };

    /^\@(c|[a-z]+index)\b/ and next;
    /^\@subsection/ and next;
    /^\@need/ and next;
    /^\@node/ and next;

    /^\@setfilename\s+([^.]+)/ and $fn = $1, next;
    /^\@settitle\s+([^.]+)/ and $tl = $1, next;

    next unless $output;

    /^\@end\s+([a-z]+)/ and do {
	if(defined $endw)
	{
	    die "\@$endw ended by \@end $1 at line $.\n"
		unless $1 eq $endw;

	    if($endw =~ /example$/)
	    {
		$shift = "";
		$_ = "";
	    }
	    elsif($endw =~ /^if/)
	    {
		$skipping = pop @skstack;
		$_ = "";
	    }
	    else
	    {
		$_ = "\n=back\n";
		$ic = pop @icstack;
	    }
	    $endw = pop @endwstack;
	}
    };

    /^\@end ignore/ and $ignore = 0, next;
    next if $ignore;
    next if $skipping;

    /^\@ignore/ and $ignore = 1, next;

    /^\@set\s+([a-zA-Z0-9_-]+)\s*(.*)$/ and $defs{$1} = $2, next;
    /^\@clear\s+([a-zA-Z0-9_-]+)/ and delete $defs{$1}, next;

    /^\@ifset\s+([a-zA-Z0-9_-]+)/ and do {
	push @endwstack, $endw;
	push @skstack, $skipping;
	$endw = "ifset";
	$skipping = 1 unless exists $defs{$1};
    };

    /^\@ifclear\s+([a-zA-Z0-9_-]+)/ and do {
	push @endwstack, $endw;
	push @skstack, $skipping;
	$endw = "ifset";
	$skipping = 1 if exists $defs{$1};
    };

    /^\@itemize (\@[a-z]+)/ and do {
	push @endwstack, $endw;
	push @icstack, $ic;
	$ic = $1;
	$ic =~ s/\@bullet/*/;
	$ic =~ s/\@minus/-/;
	$_ = "\n=over 4\n";
	$endw = "itemize";
    };

    /^\@enumerate\s+([A-Z0-9]+)/ and do {
	push @endwstack, $endw;
	push @icstack, $ic;
	$ic = $1 . ".";
	$_ = "\n=over 4\n";
	$endw = "enumerate";
    };

    /^\@table\s+(\@[a-z]+)/ and do {
	push @endwstack, $endw;
	push @icstack, $ic;
	$ic = $1;
	$ic =~ s/\@(?:samp|strong|key)/B/;
	$ic =~ s/\@(?:code|kbd)/C/;
	$ic =~ s/\@(?:dfn|var|emph|cite)/I/;
	$ic =~ s/\@(?:file)/F/;
	$_ = "\n=over 4\n";
	$endw = "table";
    };

    /^\@((?:small)?example)/ and do {
	push @endwstack, $endw;
	$endw = $1;
	$shift = "\t";
	next;
    };

    /^\@itemx?\s*(.+)?$/ and do {
	if(defined $1)
	{
	    $_ = "=item $ic\&LT;$1\&GT;\n";
	}
	else
	{
	    $_ = "=item $ic\n";
	    $ic =~ y/A-Ya-y1-8/B-Zb-z2-9/;
	}
    };

    /^\@section\s+(.+)$/ and do {
	$_ = "\n=head2 $1\n";
    };

    # POD doesn't interpret E<> inside a verbatim block.
    if ($shift eq "") {
	s/</&lt;/g;
	s/>/&gt;/g;
    } else {
	s/</&LT;/g;
	s/>/&GT;/g;
    }
    s/\@\{/&lbrace;/g;
    s/\@\}/&rbrace;/g;
    $section .= $shift.$_."\n";
}

$sects{NAME} = "$fn \- $tl\n";

for $sect (qw(NAME SYNOPSIS DESCRIPTION OPTIONS ENVIRONMENT FILES
	      BUGS NOTES SEEALSO AUTHOR COPYRIGHT))
{
    if(exists $sects{$sect})
    {
	$head = $sect;
	$head =~ s/SEEALSO/SEE ALSO/;
	print "=head1 $head\n\n";
	print $sects{$sect};
	print "\n";
    }
}

sub usage
{
    die "usage: $0 [-D toggle...] [infile [outfile]]\n";
}
