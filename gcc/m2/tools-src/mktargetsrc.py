#!/usr/bin/env python3

# maketargetsrc creates target library target modules.

# Copyright (C) 2026 Free Software Foundation, Inc.
#
# This file is part of GNU Modula-2.
#
# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Modula-2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

import argparse
import uuid
import os
import pathlib
import sys


exit_code = 0
minimalCommandLine = "-fno-scaffold-main -fno-scaffold-dynamic"
minimalCommandLine += " -fno-scaffold-static -fno-m2-plugin"
sourceLineLength = 60


def printf (format, *args):
    # printf a very basic printf.
    print(str(format) % args, end=' ')


def safeRemove(filename):
    # Remove filename if the name is not None and the filename exists.
    if (filename != None) and os.path.exists(filename):
        os.remove(filename)


def quietSystem(args, commandLine, temporaryFile):
    # Execute commandline and exit if unsuccessful.  It will run
    # gdb on the command line if --gdb is set in args.
    if args.gdb:
        result = os.system(commandLine + " -wrapper gdb,--args")
    else:
        result = os.system(commandLine + " 2>&1 /dev/null")
    if result != 0:
        safeRemove(temporaryFile)
        safeRemove(args.outputfile)
        printf("failed to execute: %s with an exit code: %d\n",
               commandLine, result)
        sys.exit(result)


def extractJoined():
    # Return the commandLine and includePath.
    commandLine = []
    includePath = ""
    defInput = ""
    modInput = ""
    compiler = ""
    i = 1
    while i<len(sys.argv):
        if (len(sys.argv[i])>2) and (sys.argv[i][:3] == '-I='):
            if includePath != "":
                includePath += " "
            includePath += "-I"
            includePath += sys.argv[i][3:]
        elif (len(sys.argv[i])>2) and (sys.argv[i][:3] == '-C='):
            compiler = sys.argv[i][3:]
        elif (len(sys.argv[i])>2) and (sys.argv[i][:3] == '-D='):
            defInput = sys.argv[i][3:]
        elif (len(sys.argv[i])>2) and (sys.argv[i][:3] == '-M='):
            modInput = sys.argv[i][3:]
        else:
            commandLine += [sys.argv[i]]
        i += 1
    return commandLine, includePath, defInput, modInput, compiler


def initOptions():
    # initOptions build and return the args object.
    commandLine, includePath, defInput, modInput, compiler = extractJoined()
    parser = argparse.ArgumentParser(prog='mktargetsrc')
    parser.add_argument('-s', '--fiso', help='use the ISO dialect',
                        default=False, action='store_true')
    parser.add_argument('-p', '--fpim', help='use the PIM dialect',
                        default=False, action='store_true')
    parser.add_argument('-g', '--gdb', help='run gm2 under gdb',
                        default=False, action='store_true')
    parser.add_argument('-o', '--outputfile', help='set the output file',
                        default=None, action='store')
    parser.add_argument('-v', '--verbose', help='generate progress messages',
                        action='store_true')
    parser.add_argument('-C=', '--compiler', help='the compiler driver',
                        default=compiler, action='store')
    parser.add_argument('-D=', '--definputfile', help='set the def input file',
                        default=defInput, action='store')
    parser.add_argument('-M=', '--modinputfile', help='set the mod input file',
                        default=modInput, action='store')
    parser.add_argument('-I=', '--include', help='append to the search path',
                        default=includePath, action='store')
    args = parser.parse_args(commandLine)
    return args, includePath


def startWith(line, sub):
    # Return True if line starts with sub.
    return (len(line)>len(sub)) and (line[:len(sub)]==sub)


def makeDictionary(temporaryFile):
    # Populate dictDefs with the contents of temporaryFile.
    dictDefs = { "SYSTEM":{}, "builtin":{} }
    for line in open(temporaryFile).readlines():
        if startWith (line, "SYSTEM module creates type:"):
            definedType = line[len("SYSTEM module creates type:"):]
            definedType = definedType.strip()
            dictDefs["SYSTEM"][definedType] = True
        elif startWith (line, "builtin procedure function:"):
            definedFunc = line[len("builtin procedure function:"):]
            definedFunc = definedFunc.strip()
            dictDefs["builtin"][definedFunc] = True
    return dictDefs


def leadingSpaces(line):
    # Return the leading number of spaces in line.
    s = line.lstrip()
    return len(line)-len(s)


def output_export_list(args, line, export_list):
    # Append the export_list to the args.outputfile.  It preserves
    # indentation and wraps at sourceLineLength.
    outf = open(args.outputfile, "a")
    indent = leadingSpaces(line)
    pos = 0
    for el in export_list:
        if pos == 0:
            outf.write(" " * indent)
            pos = indent
        outf.write(el)
        outf.write(", ")
        pos += len(el)
        pos += 2
        if pos > sourceLineLength:
            outf.write("\n")
            pos = 0
    if pos > 0:
        outf.write("\n")
    outf.close()


def output_SYSTEM_DATATYPES(args, line, dictDefs):
    # Write a list of system datatypes into the output file.
    output_export_list (args, line, [key for key in dictDefs["SYSTEM"]])


def output_SYSTEM_TYPES(args, line, dictDefs):
    # Write a list of system types into the output file.
    outf = open(args.outputfile, "a")
    indent = leadingSpaces(line)
    for key in dictDefs["SYSTEM"]:
        outf.write(" " * indent)
        outf.write(key)
        outf.write(" ;\n")
    outf.close()


def output(args, line):
    # Append line to the output file.
    outf = open(args.outputfile, "a")
    outf.write(line)
    outf.close()


def contains(line, sub):
    # Return True if line contains sub.
    return line.find(sub) >= 0


def contains_builtin_tag(line):
    # Return True if line contains a builtin tag.
    for key in builtin_func:
        if contains(line, key):
            return True
    return False


class builtin_prototype:
    def __init__(self, name, returnType, argNameList, argTypeList):
        self.name = name
        self.builtinName = "__builtin_" + name
        self.returnType = returnType
        self.argNameList = argNameList
        self.argTypeList = argTypeList

# The list of gcc_builtins which might not be available on a target.

list_of_builtins = [builtin_prototype("csinl", "LONGCOMPLEX", ["z"], ["LONGCOMPLEX"]),
                    builtin_prototype("csin", "COMPLEX", ["z"], ["COMPLEX"]),
                    builtin_prototype("csinf", "SHORTCOMPLEX", ["z"], ["SHORTCOMPLEX"]),
                    builtin_prototype("ccosl", "LONGCOMPLEX", ["z"], ["LONGCOMPLEX"]),
                    builtin_prototype("ccos", "COMPLEX", ["z"], ["COMPLEX"]),
                    builtin_prototype("ccosf", "SHORTCOMPLEX", ["z"], ["SHORTCOMPLEX"]),
                    builtin_prototype("ctanl", "LONGCOMPLEX", ["z"], ["LONGCOMPLEX"]),
                    builtin_prototype("ctan", "COMPLEX", ["z"], ["COMPLEX"]),
                    builtin_prototype("ctanf", "SHORTCOMPLEX", ["z"], ["SHORTCOMPLEX"])]

gcc_builtins = {element.builtinName: element for element in list_of_builtins}


def output_def_cbuiltin(args, keyname):
    # Generate the definition module procedure declaration for a cbuiltin keyname.
    output(args, "(* " + gcc_builtins[keyname].name + " is available on the target.  *)\n")
    output(args, "PROCEDURE %s (%s: %s) : %s ;\n" % (gcc_builtins[keyname].name,
                                                     gcc_builtins[keyname].argNameList[0],
                                                     gcc_builtins[keyname].argTypeList[0],
                                                     gcc_builtins[keyname].returnType))


def output_def_builtin(args, keyname):
    # Generate the definition module procedure declaration for a builtin keyname.
    output(args, "(* " + gcc_builtins[keyname].name + " is available on the target.  *)\n")
    output(args, "PROCEDURE __BUILTIN__ %s (%s: %s) : %s ;\n" % (gcc_builtins[keyname].name,
                                                                 gcc_builtins[keyname].argNameList[0],
                                                                 gcc_builtins[keyname].argTypeList[0],
                                                                 gcc_builtins[keyname].returnType))


def process_def_builtin(args, line, key, dictDefs):
    # Check target availability for the builtin and generate a procedure declaration
    # or an unavailablity comment.
    line = line.strip()
    if startWith(line, "(*") and (len(line)>len("(*")):
        line = line[2:]
        funcname = line.split('(')[1].split(',')[0]
        keyname = line.split('(')[1].split(',')[3]
        if keyname in dictDefs["builtin"]:
            output_def_builtin(args, keyname)
        else:
            unavailable(args, funcname)


def output_mod_builtin(args, keyname, line):
    output(args, "(* " + gcc_builtins[keyname].name + " is available on the target.  *)\n")
    output(args, "PROCEDURE __ATTRIBUTE__ __BUILTIN__ ")
    output(args, "((%s)) %s (%s: %s) : %s ;" % (gcc_builtins[keyname].builtinName,
                                                gcc_builtins[keyname].name,
                                                gcc_builtins[keyname].argNameList[0],
                                                gcc_builtins[keyname].argTypeList[0],
                                                gcc_builtins[keyname].returnType))
    builtin = line.split('(')[1].split(',')[4].split(')')[0]
    output(args, """
BEGIN
   RETURN %s(z)
END %s ;

""" % (builtin, gcc_builtins[keyname].name))


def process_mod_builtin(args, line, key, dictDefs):
    line = line.strip()
    if startWith(line, "(*") and (len(line)>len("(*")):
        line = line[2:]
        funcname = line.split('(')[1].split(',')[0]
        keyname = line.split('(')[1].split(',')[3]
        if keyname in dictDefs["builtin"]:
            output_mod_builtin(args, keyname, line)
        else:
            unavailable(args, funcname)


def unavailable(args, funcname):
    output(args, "(* Procedure function %s is unavailable on the target.  *)\n" % funcname)


def process_def_cbuiltin(args, line, key, dictDefs):
    line = line.strip()
    if startWith(line, "(*") and (len(line)>len("(*")):
        line = line[2:]
        funcname = line.split('(')[1].split(',')[0]
        keyname = line.split('(')[1].split(',')[3]
        output(args, "(* " + funcname + " is available on the target.  *)\n")
        if keyname in dictDefs["builtin"]:
            output_def_cbuiltin(args, keyname)
        else:
            unavailable(args, funcname)


def target_builtins(args, key, dictDefs):
    builtin_list = [gcc_builtins[key] for key in gcc_builtins]
    target_list = []
    for el in builtin_list:
        if el.builtinName in dictDefs["builtin"]:
            target_list += [el.name]
    return target_list


def process_cbuiltin_export(args, line, key, dictDefs):
    output_export_list (args, line, target_builtins (args, key, dictDefs))


def process_mod_target_procedure(args, line, key, dictDefs):
    line = line.strip()
    if startWith(line, "(*") and (len(line)>len("(*")):
        line = line[2:]
        funcname = line.split('(')[1].split(',')[0]
        argtype = line.split('(')[1].split(',')[1]
        returntype = line.split('(')[1].split(',')[2]
        keyname = line.split('(')[1].split(',')[3]
        cbuiltin = line.split('(')[1].split(',')[4].split('>')[0].split(')')[0]
        if keyname in dictDefs["builtin"]:
            output(args, """
PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((%s)) %s (z: %s): %s ;
BEGIN
   RETURN %s (z)
END %s ;

""" % (keyname, funcname, argtype, returntype, cbuiltin, funcname))


def process_def_target_procedure(args, line, key, dictDefs):
    line = line.strip()
    if startWith(line, "(*") and (len(line)>len("(*")):
        line = line[2:]
        funcname = line.split('(')[1].split(',')[0]
        argtype = line.split('(')[1].split(',')[1]
        returntype = line.split('(')[1].split(',')[2]
        keyname = line.split('(')[1].split(',')[3]
        if keyname in dictDefs["builtin"]:
            output(args, "PROCEDURE %s (z: %s) : %s ;\n" % (funcname, argtype, returntype))


builtin_func = { "<DEF_CBUILTIN_PROCEDURE":process_def_cbuiltin,
                 "<DEF_BUILTIN_PROCEDURE":process_def_builtin,
                 "<MOD_BUILTIN_PROCEDURE":process_mod_builtin,
                 "<CBUILTIN_EXPORT_LIST>":process_cbuiltin_export,
                 "<DEF_TARGET_PROCEDURE":process_def_target_procedure,
                 "<MOD_TARGET_PROCEDURE":process_mod_target_procedure}


def process_tag(args, line, dictDefs):
    for key in builtin_func:
        if contains(line, key):
            builtin_func[key](args, line, key, dictDefs)


def generateSource(args, temporaryFile):
    # generateSource interpet and process any tags.
    dictDefs = makeDictionary(temporaryFile)
    seen_SYSTEM_DATATYPES = False
    seen_SYSTEM_TYPES = False
    filename = args.modinputfile
    if args.definputfile != "":
        filename = args.definputfile
    if os.path.isfile(filename):
        for line in open(filename):
            if contains (line, "<SYSTEM_DATATYPES>"):
                seen_SYSTEM_DATATYPES = True
            elif contains (line, "</SYSTEM_DATATYPES>"):
                output_SYSTEM_DATATYPES(args, line, dictDefs)
                seen_SYSTEM_DATATYPES = False
            elif contains (line, "<SYSTEM_TYPES>"):
                seen_SYSTEM_TYPES = True
            elif contains (line, "</SYSTEM_TYPES>"):
                output_SYSTEM_TYPES(args, line, dictDefs)
                seen_SYSTEM_TYPES = False
            elif seen_SYSTEM_DATATYPES:
                pass
            elif contains_builtin_tag(line):
                process_tag(args, line, dictDefs)
            else:
                output(args, line)
    else:
        printf("mktargetsrc cannot open: %s\n", filename)
        sys.exit(1)


def makeTemporaryFile():
    # makeTemporaryFile returns the name of a temporary file.
    return str(uuid.uuid4())+'.txt'


def stripBackslash(compiler):
    compiler = compiler.replace("\\ ", " ")
    return compiler


def determineBuiltins(args, includePath):
    # determineBuiltins build the builtin_dict with the gm2 builtins.
    print(args.modinputfile)
    if args.outputfile != None:
        open(args.outputfile, "w").close()
    temporaryFile = makeTemporaryFile()
    dialect = '-fpim'
    if args.fiso or (not args.fpim):
        dialect = '-fiso'
    compiler = stripBackslash(args.compiler)
    commandLine = "%s %s %s %s -S -fdump-system-exports -o /dev/null %s > %s" % (
        compiler, dialect, includePath, minimalCommandLine,
        args.modinputfile, temporaryFile)
    quietSystem(args, commandLine, temporaryFile)
    commandLine = "%s %s %s %s -S -fdump-builtins %s >> %s" % (
        compiler, dialect, includePath, minimalCommandLine,
        args.modinputfile, temporaryFile)
    quietSystem(args, commandLine, temporaryFile)
    generateSource(args, temporaryFile)
    os.remove(temporaryFile)


def main():
    args, includePath = initOptions()
    determineBuiltins(args, includePath)


main()
