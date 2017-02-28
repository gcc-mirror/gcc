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

    print "static const struct processors all_cores[] ="
    print "{"

    ncpus = split (cpu_list, cpus)

    for (n = 1; n <= ncpus; n++) {
	print "  {"
	print "    \"" cpus[n] "\","
	if (cpus[n] in cpu_tune_for) {
	    if (! (cpu_tune_for[cpus[n]] in cpu_cnames)) {
		fatal("unknown \"tune for\" target " cpu_tune_for[cpus[n]] \
		      " for CPU " cpus[n])
	    }
	    print "    TARGET_CPU_" cpu_cnames[cpu_tune_for[cpus[n]]] ","
	} else {
	    print "    TARGET_CPU_" cpu_cnames[cpus[n]] ","
	}
	if (cpus[n] in cpu_tune_flags) {
	    print "    (" cpu_tune_flags[cpus[n]] "),"
	} else print "    0,"
	if (! (cpu_arch[cpus[n]] in arch_isa)) {
	    fatal("unknown arch " cpu_arch[cpus[n]] " for cpu " cpus[n])
	}
	print "    \"" arch_base[cpu_arch[cpus[n]]] "\", BASE_ARCH_" \
	    arch_base[cpu_arch[cpus[n]]] ","
	print "    {"
	print "      " arch_isa[cpu_arch[cpus[n]]] ","
	if (cpus[n] in cpu_fpu) print "      " fpu_isa[cpu_fpu[cpus[n]]] ","
	if (cpus[n] in cpu_isa) print "      " cpu_isa[cpus[n]] ","
	print "      isa_nobit"
	print "    },"
	print "    &arm_" cpu_cost[cpus[n]] "_tune"
	print "  },"
    }

    print "  {NULL, TARGET_CPU_arm_none, 0, NULL, BASE_ARCH_0," \
	" {isa_nobit}, NULL}"
    print "};\n"

    print "static const struct processors all_architectures[] ="
    print "{"

    narchs = split (arch_list, archs)

    for (n = 1; n <= narchs; n++) {
	print "  {"
	if (! (arch_tune_for[archs[n]] in cpu_cnames)) {
	    fatal("unknown \"tune for\" target " arch_tune_for[archs[n]] \
		  " for architecture " archs[n])
	}
	print "    \"" archs[n] \
	    "\", TARGET_CPU_" cpu_cnames[arch_tune_for[archs[n]]] ","
	if (archs[n] in arch_tune_flags) {
	    print "    (" arch_tune_flags[archs[n]] "),"
	} else print "    0,"
	print "    \"" arch_base[archs[n]] "\", BASE_ARCH_" \
	    arch_base[archs[n]] ","
	print "    {"
	print "      " arch_isa[archs[n]] ","
	print "      isa_nobit"
	print "    },"
	print "    NULL"
	print "  },"
    }

    print "  {NULL, TARGET_CPU_arm_none, 0, NULL, BASE_ARCH_0," \
	" {isa_nobit}, NULL}"
    print "};\n"

    print "const struct arm_fpu_desc all_fpus[] ="
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

function gen_comm_data () {
    boilerplate("C")

    print "static const struct arm_arch_core_flag arm_arch_core_flags[] ="
    print "{"

    ncpus = split (cpu_list, cpus)

    for (n = 1; n <= ncpus; n++) {
	print "  {"
	print "    \"" cpus[n] "\","
	if (! (cpu_arch[cpus[n]] in arch_isa)) {
	    fatal("unknown arch " cpu_arch[cpus[n]] " for cpu " cpus[n])
	}
	print "    {"
	print "      " arch_isa[cpu_arch[cpus[n]]] ","
	if (cpus[n] in cpu_fpu)	print "      " fpu_isa[cpu_fpu[cpus[n]]] ","
	if (cpus[n] in cpu_isa)	print "      " cpu_isa[cpus[n]] ","
	print "      isa_nobit"
	print "    },"
	print "  },"
    }

    narchs = split (arch_list, archs)

    for (n = 1; n <= narchs; n++) {
	print "  {"
	print "    \"" archs[n] "\","
	print "    {"
	print "      " arch_isa[archs[n]] ","
	print "      isa_nobit"
	print "    },"
	print "  },"
    }

    print "};\n"
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
    if (name in cpu_cnames) {
	print cpu_cnames[name]
    } else print "error"
}

function check_fpu (name) {
    if (name in fpu_cnames) {
	print fpu_cnames[name]
    } else print "error"
}

function check_arch (name) {
    if (name in arch_isa) {
	print name
    } else print "error"
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

/^end arch / {
    if (arch_name != $3) fatal("mimatched end arch")
    if (! arch_name in arch_tune_for) {
	fatal("arch definition lacks a \"tune for\" statement")
    }
    if (! arch_name in arch_isa) {
	fatal("arch definition lacks an \"isa\" statement")
    }
    arch_list = arch_list " " arch_name
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
	check_cpu(target[2])
    } else if (cmd ~ /^chkarch /) {
	split (cmd, target)
	check_arch(target[2])
    } else if (cmd ~ /^chkfpu /) {
	split (cmd, target)
	check_fpu(target[2])
    } else fatal("unrecognized command: "cmd)
}
