# Manipulate the CPU, FPU and architecture descriptions for ARM.
# Copyright (C) 2017 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Invoke this with '-v cmd=<cmd>"
# where <cmd> is one of:
#	data: Print the standard 'C' data tables for the CPUs
#	common-data: Print the 'C' data for shared driver/compiler files
#	headers: Print the standard 'C' headers for the CPUs
#	md: Print the machine description fragment
#	opt: Print the option tables fragment
#	chkcpu <name>: Checks that <name> is a valid CPU
#	chktune <name>: Checks that <name> is a valid CPU
#	chkfpu <name>: Checks that <name> is a valid FPU
#	chkarch <name>: Checks that <arch> is a valid architecture

function fatal (m) {
    print "error ("lineno"): " m > "/dev/stderr"
    exit 1
}

function toplevel () {
    if (cpu_name != "") fatal("missing \"end cpu\"")
    if (arch_name != "") fatal("missing \"end arch\"")
    if (fpu_name != "") fatal("missing \"end fpu\"")
}

function boilerplate (style) {
    ce = ""
    if (style == "C" ) {
	cs = "/* "
	cc = "   "
	ce = "  */"
    } else if (style == "md") {
	cc = "; "
	cs = cc
    } else if (style == "sh") {
	cc = "# "
	cs = cc
    } else fatal("Unknown comment style: "style)

    print cs "-*- buffer-read-only: t -*-"

    print cc "Generated automatically by parsecpu.awk from arm-cpus.in."
    print cc "Do not edit."
    print ""
    print cc "Copyright (C) 2011-2017 Free Software Foundation, Inc."
    print ""
    print cc "This file is part of GCC."
    print ""
    print cc "GCC is free software; you can redistribute it and/or modify"
    print cc "it under the terms of the GNU General Public License as"
    print cc "published by the Free Software Foundation; either version 3,"
    print cc "or (at your option) any later version."
    print ""
    print cc "GCC is distributed in the hope that it will be useful,"
    print cc "but WITHOUT ANY WARRANTY; without even the implied warranty of"
    print cc "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    print cc "GNU General Public License for more details."
    print ""
    print cc "You should have received a copy of the GNU General Public"
    print cc "License along with GCC; see the file COPYING3.  If not see"
    print cc "<http://www.gnu.org/licenses/>." ce
    print ""
}

function tune_flag_pfx (f) {
    return "TF_" f
}

function isa_pfx (f) {
    if (f ~ /^(bit|quirk)_.*/) return "isa_" f
    return "ISA_" f
}

function gen_headers () {
    boilerplate("C")

    print "enum processor_type"
    print "{"

    ncpus = split (cpu_list, cpus)

    for (n = 1; n <= ncpus; n++) {
	print "  TARGET_CPU_"cpu_cnames[cpus[n]]","
    }
    print "  TARGET_CPU_arm_none"
    print "};\n"

    print "enum arch_type"
    print "{"

    narchs = split (arch_list, archs)

    for (n = 1; n <= narchs; n++) {
	print "  TARGET_ARCH_"arch_cnames[archs[n]]","
    }
    print "  TARGET_ARCH_arm_none"
    print "};\n"

    print "enum fpu_type"
    print "{"

    nfpus = split (fpu_list, fpus)

    for (n = 1; n <= nfpus; n++) {
	print "  TARGET_FPU_"fpu_cnames[fpus[n]]","
    }
    print "  TARGET_FPU_auto"
    print "};"
}

function gen_data () {
    boilerplate("C")

    print "static const cpu_tune all_tunes[] ="
    print "{"

    ncpus = split (cpu_list, cpus)

    for (n = 1; n <= ncpus; n++) {
	print "  { /* " cpus[n] ".  */"
	# scheduler
	if (cpus[n] in cpu_tune_for) {
	    if (! (cpu_tune_for[cpus[n]] in cpu_cnames)) {
		fatal("unknown \"tune for\" target " cpu_tune_for[cpus[n]] \
		      " for CPU " cpus[n])
	    }
	    print "    TARGET_CPU_" cpu_cnames[cpu_tune_for[cpus[n]]] ","
	} else {
	    print "    TARGET_CPU_" cpu_cnames[cpus[n]] ","
	}
	# tune_flags
	if (cpus[n] in cpu_tune_flags) {
	    print "    (" cpu_tune_flags[cpus[n]] "),"
	} else print "    0,"
	# tune
	print "    &arm_" cpu_cost[cpus[n]] "_tune"
	print "  },"
    }
    print "  {TARGET_CPU_arm_none, 0, NULL}"
    print "};"
    
}

function gen_comm_data () {
    boilerplate("C")

    ncpus = split (cpu_list, cpus)

    for (n = 1; n <= ncpus; n++) {
	if (cpus[n] in cpu_opts) {
	    print "static const cpu_arch_extension cpu_opttab_" \
		cpu_cnames[cpus[n]] "[] = {"
	    nopts = split (cpu_opts[cpus[n]], opts)
	    for (opt = 1; opt <= nopts; opt++) {
		print "  {"
		print "    \"" opts[opt] "\", " \
		    cpu_opt_remove[cpus[n],opts[opt]] ", false,"
		print "    { " cpu_opt_isa[cpus[n],opts[opt]] ", isa_nobit }"
		print "  },"
	    }
	    if (cpus[n] in cpu_optaliases) {
		naliases = split (cpu_optaliases[cpus[n]], aliases)
		for (alias = 1; alias <= naliases; alias++) {
		    if (! ((cpus[n], \
			    cpu_opt_alias[cpus[n],aliases[alias]]) in \
			   cpu_opt_isa)) {
			fatal("Alias " aliases[alias] " target not defined " \
			      "for CPU " cpus[n])
		    }
		    equiv=cpu_opt_alias[cpus[n],aliases[alias]]
		    print "  {"
		    print "    \"" aliases[alias] "\", " \
			cpu_opt_remove[cpus[n],equiv] ", true, "
		    print "    { " cpu_opt_isa[cpus[n],equiv] ", isa_nobit }"
		    print "  },"
		}
	    }
	    print "  { NULL, false, false, {isa_nobit}}"
	    print "};\n"
	}
    }

    print "const cpu_option all_cores[] ="
    print "{"

    for (n = 1; n <= ncpus; n++) {
	print "  {"
	print "    {"
	# common.name
	print "      \"" cpus[n] "\","
	# common.extensions
	if (cpus[n] in cpu_opts) {
	    print "      cpu_opttab_" cpu_cnames[cpus[n]] ","
	} else print "      NULL,"
	# common.isa_bits
	nfeats = split (cpu_arch[cpus[n]], feats, "+")
	if (! (feats[1] in arch_isa)) {
	    fatal("unknown arch " feats[1] " for cpu " cpus[n])
	}
	print "      {"
	print "        " arch_isa[feats[1]] ","
	for (m = 2; m <= nfeats; m++) {
	    if (! ((feats[1], feats[m]) in arch_opt_isa)) {
		fatal("unknown feature " feats[m] " for architecture " feats[1])
	    }
	    if (arch_opt_remove[feats[1],feats[m]] == "true") {
		fatal("cannot remove features from architecture specs")
	    }
	    # The isa_features array that is being initialized here has a length
	    # of max isa_bit_num, which is the last entry in the enum.
	    # Logically this means that the number of features is implicitly
	    # never more than the number of feature bits we have.  This is only
	    # true if we don't emit duplicates here.  So keep track of which
	    # options we have already emitted so we don't emit them twice.
	    nopts = split (arch_opt_isa[feats[1],feats[m]], opts, ",")
	    for (i = 1; i <= nopts; i++) {
		if (! (opts[i] in seen)) {
		  print "        " opts[i] ","
		  seen[opts[i]]
		}
	    }
	}
	if (cpus[n] in cpu_fpu) {
	    nopts = split (fpu_isa[cpu_fpu[cpus[n]]], opts, ",")
	    for (i = 1; i <= nopts; i++) {
		if (! (opts[i] in seen)) {
		  print "        " opts[i] ","
		  seen[opts[i]]
		}
	    }
	}
	if (cpus[n] in cpu_isa) {
	    nopts = split (cpu_isa[cpus[n]], opts, ",")
	    for (i = 1; i <= nopts; i++) {
		if (! (opts[i] in seen)) {
		  print "        " opts[i] ","
		  seen[opts[i]]
		}
	    }
	}
	delete seen
	print "        isa_nobit"
	print "      }"
	print "    },"
	# arch
	print "    TARGET_ARCH_" arch_cnames[feats[1]]
	print "  },"
    }

    print "  {{NULL, NULL, {isa_nobit}}, TARGET_ARCH_arm_none}"
    print "};"

    narchs = split (arch_list, archs)

    for (n = 1; n <= narchs; n++) {
	if (archs[n] in arch_opts) {
	    print "static const struct cpu_arch_extension arch_opttab_" \
		arch_cnames[archs[n]] "[] = {"
	    nopts = split (arch_opts[archs[n]], opts)
	    for (opt = 1; opt <= nopts; opt++) {
		print "  {"
		print "    \"" opts[opt] "\", " \
		    arch_opt_remove[archs[n],opts[opt]] ", false,"
		print "    { " arch_opt_isa[archs[n],opts[opt]] ", isa_nobit }"
		print "  },"
	    }
	    if (archs[n] in arch_optaliases) {
		naliases = split (arch_optaliases[archs[n]], aliases)
		for (alias = 1; alias <= naliases; alias++) {
		    if (! ((archs[n], \
			    arch_opt_alias[archs[n],aliases[alias]]) in \
			   arch_opt_isa)) {
			fatal("Alias " aliases[alias] " target not defined " \
			      "for architecture " archs[n])
		    }
		    equiv=arch_opt_alias[archs[n],aliases[alias]]
		    print "  {"
		    print "    \"" aliases[alias] "\", " \
			arch_opt_remove[archs[n],equiv] ", true, "
		    print "    { " arch_opt_isa[archs[n],equiv] ", isa_nobit }"
		    print "  },"
		}
	    }
	    print "  { NULL, false, false, {isa_nobit}}"
	    print "};\n"
	} else if (archs[n] in arch_optaliases) {
	    fatal("Architecture " archs[n] " has option aliases but no options")
	}
    }

    print "const arch_option all_architectures[] ="
    print "{"

    for (n = 1; n <= narchs; n++) {
	print "  {"
	if (! (arch_tune_for[archs[n]] in cpu_cnames)) {
	    fatal("unknown \"tune for\" target " arch_tune_for[archs[n]] \
		  " for architecture " archs[n])
	}
	# common.name
	print "    \"" archs[n] "\","
	# common.extensions
	if (archs[n] in arch_opts) {
	    print "    arch_opttab_" arch_cnames[archs[n]] ","
	} else print "    NULL,"
	# common.isa_bits
	print "    {"
	print "      " arch_isa[archs[n]] ","
	print "      isa_nobit"
	print "    },"
	# arch, base_arch
	print "    \"" arch_base[archs[n]] "\", BASE_ARCH_" \
	    arch_base[archs[n]] ","
	# profile letter code, or zero if none.
	if (archs[n] in arch_prof) {
	    print "    '" arch_prof[archs[n]] "',"
	} else {
	    print "    0,"
	}
	# tune_id
	print "    TARGET_CPU_" cpu_cnames[arch_tune_for[archs[n]]] ","
	print "  },"
    }

    print "  {{NULL, NULL, {isa_nobit}},"
    print "   NULL, BASE_ARCH_0, 0, TARGET_CPU_arm_none}"
    print "};\n"

    print "const arm_fpu_desc all_fpus[] ="
    print "{"

    nfpus = split (fpu_list, fpus)

    for (n = 1; n <= nfpus; n++) {
	print "  {"
	print "    \"" fpus[n] "\","
	print "    {"
	print "      " fpu_isa[fpus[n]] ","
	print "      isa_nobit"
	print "    }"
	print "  },"
    }

    print "};"
}

function gen_md () {
    boilerplate("md")

    z = ORS
    ORS = ""
    print "(define_attr \"tune\"\n\t\""

    ncpus = split (cpu_list, cpus)

    for (n = 1; n < ncpus; n++) {
	if ((n % 3) != 0) {
	    ORS = ","
	} else ORS = ",\n\t"
	print cpu_cnames[cpus[n]]
    }
    ORS = z
    print cpu_cnames[cpus[ncpus]]"\""
    print "\t(const (symbol_ref \"((enum attr_tune) arm_tune)\")))"
}

function gen_opt () {
    boilerplate("md")

    print "Enum"
    print "Name(processor_type) Type(enum processor_type)"
    print "Known ARM CPUs (for use with the -mcpu= and -mtune= options):\n"

    ncpus = split (cpu_list, cpus)

    for (n = 1; n <= ncpus; n++) {
	print "EnumValue"
	print "Enum(processor_type) String(" cpus[n] \
	    ") Value( TARGET_CPU_"cpu_cnames[cpus[n]]")"
	print ""
    }

    print "Enum"
    print "Name(arm_arch) Type(int)"
    print "Known ARM architectures (for use with the -march= option):\n"

    narchs = split (arch_list, archs)

    for (n = 1; n <= narchs; n++) {
	print "EnumValue"
	print "Enum(arm_arch) String(" archs[n] \
	    ") Value("n - 1")"
	print ""
    }

    print "Enum"
    print "Name(arm_fpu) Type(enum fpu_type)"
    print "Known ARM FPUs (for use with the -mfpu= option):\n"

    nfpus = split (fpu_list, fpus)

    for (n = 1; n <= nfpus; n++) {
	print "EnumValue"
	print "Enum(arm_fpu) String(" fpus[n] \
	    ") Value(TARGET_FPU_"fpu_cnames[fpus[n]]")"
	print ""
    }

    print "EnumValue"
    print "Enum(arm_fpu) String(auto) Value(TARGET_FPU_auto)"
}

function check_cpu (name) {
    exts = split (name, extensions, "+")

    if (! extensions[1] in cpu_cnames) {
	return "error"
    }

    for (n = 2; n <= exts; n++) {
	if (!((extensions[1], extensions[n]) in cpu_opt_remove)	\
	    && !((extensions[1], extensions[n]) in cpu_optaliases)) {
	    return "error"
	}
    }
    return name
}

function check_fpu (name) {
    if (name in fpu_cnames) {
	print fpu_cnames[name]
    } else print "error"
}

function check_arch (name) {
    exts = split (name, extensions, "+")

    if (! extensions[1] in arch_isa) {
	return "error"
    }

    for (n = 2; n <= exts; n++) {
	if (!((extensions[1], extensions[n]) in arch_opt_remove)	\
	    && !((extensions[1], extensions[n]) in arch_optaliases)) {
	    return "error"
	}
    }
    return name
}

BEGIN {
    cpu_name = ""
    arch_name = ""
    fpu_name = ""
    lineno = 0
    if (cmd == "") fatal("Usage parsecpu.awk -v cmd=<xyz>")
}

// {
    lineno++
    parse_ok = 0
}

/^#/ {
    parse_ok = 1
}

/^begin fpu / {
    toplevel()
    fpu_name = $3
    parse_ok = 1
}

/^end fpu / {
    if (fpu_name != $3) fatal("mimatched end fpu")
    if (! (fpu_name in fpu_isa)) {
	fatal("fpu definition \"" fpu_name "\" lacks an \"isa\" statement")
    }
    fpu_cnames[fpu_name] = fpu_name
    gsub(/[-+.]/, "_", fpu_cnames[fpu_name])
    fpu_list = fpu_list " " fpu_name
    fpu_name = ""
    parse_ok = 1
}

/^begin arch / {
    toplevel()
    arch_name = $3
    parse_ok = 1
}

/^[ 	]*base / {
    if (arch_name == "") fatal("\"base\" statement outside of arch block")
    arch_base[arch_name] = $2
    parse_ok = 1
}

/^[ 	]*profile / {
    if (arch_name == "") fatal("\"profile\" statement outside of arch block")
    arch_prof[arch_name] = $2
    parse_ok = 1
}

/^end arch / {
    if (arch_name != $3) fatal("mimatched end arch")
    if (! arch_name in arch_tune_for) {
	fatal("arch definition lacks a \"tune for\" statement")
    }
    if (! arch_name in arch_isa) {
	fatal("arch definition lacks an \"isa\" statement")
    }
    arch_list = arch_list " " arch_name
    arch_cnames[arch_name] = arch_name
    gsub(/[-+.]/, "_", arch_cnames[arch_name])
    arch_name = ""
    parse_ok = 1
}

/^begin cpu / {
    toplevel()
    cpu_name = $3
    parse_ok = 1
}

/^[ 	]*cname / {
    if (cpu_name == "") fatal("\"cname\" outside of cpu block")
    cpu_cnames[cpu_name] = $2
    parse_ok = 1
}

/^[ 	]*tune for / {
    if (cpu_name != "") {
	cpu_tune_for[cpu_name] = $3
    } else if (arch_name != "") {
	arch_tune_for[arch_name] = $3
    } else fatal("\"tune for\" outside of cpu or arch block")
    parse_ok = 1
}

/^[ 	]*tune flags / {
    flags=""
    flag_count = NF
    for (n = 3; n <= flag_count; n++) {
	if (n == 3) {
	    flags = tune_flag_pfx($n)
	} else flags = flags " | " tune_flag_pfx($n)
    }
    if (cpu_name != "") {
	cpu_tune_flags[cpu_name] = flags
    } else if (arch_name != "") {
	arch_tune_flags[arch_name] = flags
    } else fatal("\"tune flags\" outside of cpu or arch block")
    parse_ok = 1
}

/^[ 	]*architecture / {
    if (cpu_name == "") fatal("\"architecture\" outside of cpu block")
    cpu_arch[cpu_name] = $2
    parse_ok = 1
}

/^[ 	]*fpu / {
    if (cpu_name == "") fatal("\"fpu\" outside of cpu block")
    cpu_fpu[cpu_name] = $2
    parse_ok = 1
}

/^[ 	]*isa / {
    flags=""
    flag_count = NF
    for (n = 2; n <= flag_count; n++) {
	if (n == 2) {
	    flags = isa_pfx($n)
	} else flags = flags "," isa_pfx($n)
    }
    if (cpu_name != "") {
	cpu_isa[cpu_name] = flags
    } else if (arch_name != "") {
	arch_isa[arch_name] = flags
    } else  if (fpu_name != "") {
	fpu_isa[fpu_name] = flags
    } else fatal("\"isa\" outside of cpu, fpu or arch block")
    parse_ok = 1
}

/^[ 	]*option / {
    name=$2
    if ($3 == "add") {
	remove = "false"
    } else if ($3 == "remove") {
	remove = "true"
    } else fatal("syntax: option <name> add|remove isa-list")
    flags=""
    flag_count = NF
    for (n = 4; n <= flag_count; n++) {
	if (n == 4) {
	    flags = isa_pfx($n)
	} else flags = flags "," isa_pfx($n)
    }
    if (cpu_name != "") {
	cpu_opts[cpu_name] = cpu_opts[cpu_name] " " name
	cpu_opt_remove[cpu_name,name] = remove
	cpu_opt_isa[cpu_name,name] = flags
    } else if (arch_name != "") {
	arch_opts[arch_name] = arch_opts[arch_name] " " name
	arch_opt_remove[arch_name,name] = remove
	arch_opt_isa[arch_name,name] = flags
    } else fatal("\"option\" outside of cpu or arch block")
    parse_ok = 1
}

/^[ 	]*optalias / {
    name=$2
    alias=$3
    if (cpu_name != "") {
	cpu_optaliases[cpu_name] = cpu_optaliases[cpu_name] " " name
	cpu_opt_alias[cpu_name,name] = alias
    } else if (arch_name != "") {
	arch_optaliases[arch_name] = arch_optaliases[arch_name] " " name
	arch_opt_alias[arch_name,name] = alias
    } else fatal("\"optalias\" outside of cpu or arch block")
    parse_ok = 1
}

/^[ 	]*costs / {
    if (cpu_name == "") fatal("\"costs\" outside of cpu block")
    cpu_cost[cpu_name] = $2
    parse_ok = 1
}

/^end cpu / {
    if (cpu_name != $3) fatal("mimatched end cpu")
    if (! (cpu_name in cpu_cnames)) {
	cpu_cnames[cpu_name] = cpu_name
	gsub(/[-+.]/, "_", cpu_cnames[cpu_name])
    }
    if (! cpu_name in cpu_arch) fatal("cpu definition lacks an architecture")
    cpu_list = cpu_list " " cpu_name
    cpu_name = ""
    parse_ok = 1
}

/[^\s]/ {
    if (! parse_ok) fatal("Unrecognized statement: " $0)
}

END {
    toplevel()
    if (cmd == "data") {
	gen_data()
    } else if (cmd == "common-data") {
	gen_comm_data()
    } else if (cmd == "headers") {
	gen_headers()
    } else if (cmd == "md") {
	gen_md()
    } else if (cmd == "opt") {
	gen_opt()
    } else if (cmd ~ /^chk(cpu|tune) /) {
	split (cmd, target)
	print check_cpu(target[2])
    } else if (cmd ~ /^chkarch /) {
	split (cmd, target)
	print check_arch(target[2])
    } else if (cmd ~ /^chkfpu /) {
	split (cmd, target)
	check_fpu(target[2])
    } else fatal("unrecognized command: "cmd)
}
