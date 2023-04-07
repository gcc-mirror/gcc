#!/usr/bin/env python3

# def2doc.py creates texi library documentation for all exported procedures.
# Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

# Copyright (C) 2000-2023 Free Software Foundation, Inc.
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
# along with GNU Modula-2; see the file COPYING.  If not, write to the
# Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.
#

import argparse
import os
import sys

Base_Libs = ['gm2-libs', 'Base libraries', 'Basic M2F compatible libraries']

PIM_Log_Desc = 'PIM and Logitech 3.0 compatible libraries'
PIM_Log = ['gm2-libs-log', 'PIM and Logitech 3.0 Compatible', PIM_Log_Desc]
PIM_Cor_Desc = 'PIM compatible process support'
PIM_Cor = ['gm2-libs-coroutines', 'PIM coroutine support', PIM_Cor_Desc]
ISO_Libs = ['gm2-libs-iso', 'M2 ISO Libraries', 'ISO defined libraries']

library_classifications = [Base_Libs, PIM_Log, PIM_Cor, ISO_Libs]

# state_states
state_none, state_var, state_type, state_const = range(4)
# block states
block_none, block_code, block_text, block_index = range(4)


class state:
    def __init__(self):
        self._state_state = state_none
        self._block = block_none

    def get_state(self):
        return self._state_state

    def set_state(self, value):
        self._state_state = value

    def is_const(self):
        return self._state_state == state_const

    def is_type(self):
        return self._state_state == state_type

    def is_var(self):
        return self._state_state == state_var

    def get_block(self):
        return self._block

    def _change_block(self, new_block):
        if self._block != new_block:
            self._block = new_block
            self._emit_block_desc()

    def _emit_block_desc(self):
        if self._block == block_code:
            output.write('.. code-block:: modula2\n')
        elif self._block == block_index:
            output.write('.. index::\n')

    def to_code(self):
        self._change_block(block_code)

    def to_index(self):
        self._change_block(block_index)


def init_state():
    global state_obj
    state_obj = state()


def emit_node(name, nxt, previous, up):
    if args.texinfo:
        output.write('@node ' + name + ', ' + nxt + ', ')
        output.write(previous + ', ' + up + '\n')
    elif args.sphinx:
        output.write('@c @node ' + name + ', ' + nxt + ', ')
        output.write(previous + ', ' + up + '\n')


def emit_section(name):
    if args.texinfo:
        output.write('@section ' + name + '\n')
    elif args.sphinx:
        output.write(name + '\n')
        output.write('=' * len(name) + '\n')


def emit_sub_section(name):
    if args.texinfo:
        output.write('@subsection ' + name + '\n')
    elif args.sphinx:
        output.write(name + '\n')
        output.write('-' * len(name) + '\n')


def display_library_class():
    # display_library_class displays a node for a library directory and invokes
    # a routine to summarize each module.
    global args
    previous = ''
    nxt = library_classifications[1][1]
    i = 0
    lib = library_classifications[i]
    while True:
        emit_node(lib[1], nxt, previous, args.up)
        emit_section(lib[1])
        output.write('\n')
        display_modules(lib[1], lib[0], args.builddir, args.sourcedir)
        output.write('\n')
        output.write('@c ' + '-' * 60 + '\n')
        previous = lib[1]
        i += 1
        if i == len(library_classifications):
            break
        lib = library_classifications[i]
        if i+1 == len(library_classifications):
            nxt = ''
        else:
            nxt = library_classifications[i+1][1]


def display_menu():
    # display_menu displays the top level menu for library documentation.
    output.write('@menu\n')
    for lib in library_classifications:
        output.write('* ' + lib[1] + '::' + lib[2] + '\n')
    output.write('@end menu\n')
    output.write('\n')
    output.write('@c ' + '=' * 60 + '\n')
    output.write('\n')


def remote_initial_comments(file, line):
    # remote_initial_comments removes any (* *) at the top
    # of the definition module.
    while (line.find('*)') == -1):
        line = file.readline()


def removeable_field(line):
    # removeable_field - returns True if a comment field should be removed
    # from the definition module.
    field_list = ['Author', 'Last edit', 'LastEdit', 'Last update',
                  'Date', 'Title', 'Revision']
    for field in field_list:
        if (line.find(field) != -1) and (line.find(':') != -1):
            return True
    ignore_list = ['System', 'SYSTEM']
    for ignore_field in ignore_list:
        if line.find(ignore_field) != -1:
            if line.find(':') != -1:
                if line.find('Description:') == -1:
                    return True
    return False


def remove_fields(file, line):
    # remove_fields removes Author/Date/Last edit/SYSTEM/Revision
    # fields from a comment within the start of a definition module.
    while (line.find('*)') == -1):
        if not removeable_field(line):
            line = line.rstrip().replace('{', '@{').replace('}', '@}')
            output.write(line + '\n')
        line = file.readline()
    output.write(line.rstrip() + '\n')


def emit_index(entry, tag):
    global state_obj
    if args.texinfo:
        if tag == '':
            output.write('@findex ' + entry.rstrip() + '\n')
        else:
            output.write('@findex ' + entry.rstrip() + ' ' + tag + '\n')
    elif args.sphinx:
        if tag == '':
            state_obj.to_index()
            output.write(' ' * 3 + entry.rstrip() + '\n')
        else:
            state_obj.to_index()
            output.write(' ' * 3 + 'pair: ' + entry.rstrip() + '; ' + tag + '\n')


def check_index(line):
    # check_index - create an index entry for a PROCEDURE, TYPE, CONST or VAR.
    global state_obj

    words = line.split()
    procedure = ''
    if (len(words) > 1) and (words[0] == 'PROCEDURE'):
        state_obj.set_state(state_none)
        if (words[1] == '__BUILTIN__') and (len(words) > 2):
            procedure = words[2]
        else:
            procedure = words[1]
    if (len(line) > 1) and (line[0:2] == '(*'):
        state_obj.set_state(state_none)
    elif line == 'VAR':
        state_obj.set_state(state_var)
        return
    elif line == 'TYPE':
        state_obj.set_state(state_type)
        return
    elif line == 'CONST':
        state_obj.set_state(state_const)
    if state_obj.is_var():
        words = line.split(',')
        for word in words:
            word = word.lstrip()
            if word != '':
                if word.find(':') == -1:
                    emit_index(word, '(var)')
                elif len(word) > 0:
                    var = word.split(':')
                    if len(var) > 0:
                        emit_index(var[0], '(var)')
    if state_obj.is_type():
        words = line.lstrip()
        if words.find('=') != -1:
            word = words.split('=')
            if (len(word[0]) > 0) and (word[0][0] != '_'):
                emit_index(word[0].rstrip(), '(type)')
        else:
            word = words.split()
            if (len(word) > 1) and (word[1] == ';'):
                # hidden type
                if (len(word[0]) > 0) and (word[0][0] != '_'):
                    emit_index(word[0].rstrip(), '(type)')
    if state_obj.is_const():
        words = line.split(';')
        for word in words:
            word = word.lstrip()
            if word != '':
                if word.find('=') != -1:
                    var = word.split('=')
                    if len(var) > 0:
                        emit_index(var[0], '(const)')
    if procedure != '':
        name = procedure.split('(')
        if name[0] != '':
            proc = name[0]
            if proc[-1] == ';':
                proc = proc[:-1]
            if proc != '':
                emit_index(proc, '')

def demangle_system_datatype(line, indent):
    # The spaces in front align in the export qualified list.
    indent += len ('EXPORT QUALIFIED ')
    line = line.replace('@SYSTEM_DATATYPES@',
                        '\n' + indent * ' ' + 'Target specific data types.')
    line = line.replace('@SYSTEM_TYPES@',
                        '(* Target specific data types.  *)')
    return line


def emit_texinfo_content(f, line):
    global state_obj
    output.write(line.rstrip() + '\n')
    line = f.readline()
    if len(line.rstrip()) == 0:
        output.write('\n')
        line = f.readline()
        if (line.find('(*') != -1):
            remove_fields(f, line)
        else:
            output.write(line.rstrip() + '\n')
    else:
        output.write(line.rstrip() + '\n')
    line = f.readline()
    while line:
        line = line.rstrip()
        check_index(line)
        line = line.replace('{', '@{').replace('}', '@}')
        line = demangle_system_datatype(line, 0)
        output.write(line + '\n')
        line = f.readline()
    return f


def emit_sphinx_content(f, line):
    global state_obj
    state_obj.to_code()
    indentation = 4
    indent = ' ' * indentation
    output.write(indent + line.rstrip() + '\n')
    line = f.readline()
    if len(line.rstrip()) == 0:
        output.write('\n')
        line = f.readline()
        if (line.find('(*') != -1):
            remove_fields(f, line)
        else:
            output.write(indent + line.rstrip() + '\n')
    else:
        output.write(indent + line.rstrip() + '\n')
    line = f.readline()
    while line:
        line = line.rstrip()
        check_index(line)
        state_obj.to_code()
        line = demangle_system_datatype(line, indentation)
        output.write(indent + line + '\n')
        line = f.readline()
    return f


def emit_example_content(f, line):
    if args.texinfo:
        return emit_texinfo_content(f, line)
    elif args.sphinx:
        return emit_sphinx_content(f, line)


def emit_example_begin():
    if args.texinfo:
        output.write('@example\n')


def emit_example_end():
    if args.texinfo:
        output.write('@end example\n')


def emit_page(need_page):
    if need_page and args.texinfo:
        output.write('@page\n')


def parse_definition(dir_, source, build, file, need_page):
    # parse_definition reads a definition module and creates
    # indices for procedures, constants, variables and types.
    output.write('\n')
    with open(find_file(dir_, build, source, file), 'r') as f:
        init_state()
        line = f.readline()
        while (line.find('(*') != -1):
            remote_initial_comments(f, line)
            line = f.readline()
        while (line.find('DEFINITION') == -1):
            line = f.readline()
        emit_example_begin()
        f = emit_example_content(f, line)
        emit_example_end()
        emit_page(need_page)


def parse_modules(up, dir_, build, source, list_of_modules):
    previous = ''
    i = 0
    if len(list_of_modules) > 1:
        nxt = dir_ + '/' + list_of_modules[1][:-4]
    else:
        nxt = ''
    while i < len(list_of_modules):
        emit_node(dir_ + '/' + list_of_modules[i][:-4], nxt, previous, up)
        emit_sub_section(dir_ + '/' + list_of_modules[i][:-4])
        parse_definition(dir_, source, build, list_of_modules[i], True)
        output.write('\n')
        previous = dir_ + '/' + list_of_modules[i][:-4]
        i = i + 1
        if i+1 < len(list_of_modules):
            nxt = dir_ + '/' + list_of_modules[i+1][:-4]
        else:
            nxt = ''


def do_cat(name):
    # do_cat displays the contents of file, name, to stdout
    with open(name, 'r') as file:
        line = file.readline()
        while line:
            output.write(line.rstrip() + '\n')
            line = file.readline()


def module_menu(dir_, build, source):
    # module_menu generates a simple menu for all definition modules
    # in dir
    output.write('@menu\n')
    list_of_files = []
    if os.path.exists(os.path.join(source, dir_)):
        list_of_files += os.listdir(os.path.join(source, dir_))
    if os.path.exists(os.path.join(source, dir_)):
        list_of_files += os.listdir(os.path.join(build, dir_))
    list_of_files = list(dict.fromkeys(list_of_files).keys())
    list_of_files.sort()
    for file in list_of_files:
        if found_file(dir_, build, source, file):
            if (len(file) > 4) and (file[-4:] == '.def'):
                output.write('* ' + dir_ + '/' + file[:-4] + '::' + file + '\n')
    output.write('@end menu\n')
    output.write('\n')


def check_directory(dir_, build, source):
    # check_directory - returns True if dir exists in either build or source.
    if os.path.isdir(build) and os.path.exists(os.path.join(build, dir_)):
        return True
    elif os.path.isdir(source) and os.path.exists(os.path.join(source, dir_)):
        return True
    else:
        return False


def found_file(dir_, build, source, file):
    # found_file return True if file is found in build/dir/file or
    # source/dir/file.
    name = os.path.join(os.path.join(build, dir_), file)
    if os.path.exists(name):
        return True
    name = os.path.join(os.path.join(source, dir_), file)
    if os.path.exists(name):
        return True
    return False


def find_file(dir_, build, source, file):
    # find_file return the path to file searching in build/dir/file
    # first then source/dir/file.
    name1 = os.path.join(os.path.join(build, dir_), file)
    if os.path.exists(name1):
        return name1
    name2 = os.path.join(os.path.join(source, dir_), file)
    if os.path.exists(name2):
        return name2
    sys.stderr.write('file cannot be found in either ' + name1)
    sys.stderr.write(' or ' + name2 + '\n')
    os.sys.exit(1)


def display_modules(up, dir_, build, source):
    # display_modules walks though the files in dir and parses
    # definition modules and includes README.texi
    if check_directory(dir_, build, source):
        if args.texinfo:
            ext = '.texi'
        elif args.sphinx:
            ext = '.rst'
        else:
            ext = ''
        if found_file(dir_, build, source, 'README' + ext):
            do_cat(find_file(dir_, build, source, 'README' + ext))
        module_menu(dir_, build, source)
        list_of_files = []
        if os.path.exists(os.path.join(source, dir_)):
            list_of_files += os.listdir(os.path.join(source, dir_))
        if os.path.exists(os.path.join(source, dir_)):
            list_of_files += os.listdir(os.path.join(build, dir_))
        list_of_files = list(dict.fromkeys(list_of_files).keys())
        list_of_files.sort()
        list_of_modules = []
        for file in list_of_files:
            if found_file(dir_, build, source, file):
                if (len(file) > 4) and (file[-4:] == '.def'):
                    list_of_modules += [file]
        list_of_modules.sort()
        parse_modules(up, dir_, build, source, list_of_modules)
    else:
        line = 'directory ' + dir_ + ' not found in either '
        line += build + ' or ' + source
        sys.stderr.write(line + '\n')


def display_copyright():
    output.write('@c Copyright (C) 2000-2023 Free Software Foundation, Inc.\n')
    output.write('@c This file is part of GNU Modula-2.\n')
    output.write("""
@c Permission is granted to copy, distribute and/or modify this document
@c under the terms of the GNU Free Documentation License, Version 1.2 or
@c any later version published by the Free Software Foundation.
""")


def collect_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-v', '--verbose', help='generate progress messages',
                        action='store_true')
    parser.add_argument('-b', '--builddir', help='set the build directory',
                        default='.', action='store')
    parser.add_argument('-f', '--inputfile', help='set the input file',
                        default=None, action='store')
    parser.add_argument('-o', '--outputfile', help='set the output file',
                        default=None, action='store')
    parser.add_argument('-s', '--sourcedir', help='set the source directory',
                        default='.', action='store')
    parser.add_argument('-t', '--texinfo',
                        help='generate texinfo documentation',
                        default=False, action='store_true')
    parser.add_argument('-u', '--up', help='set the up node',
                        default='', action='store')
    parser.add_argument('-x', '--sphinx', help='generate sphinx documentation',
                        default=False, action='store_true')
    args = parser.parse_args()
    return args


def handle_file():
    if args.inputfile is None:
        display_copyright()
        display_menu()
        display_library_class()
    else:
        parse_definition('.', args.sourcedir, args.builddir,
                         args.inputfile, False)


def main():
    global args, output
    args = collect_args()
    if args.outputfile is None:
        output = sys.stdout
        handle_file()
    else:
        with open(args.outputfile, 'w') as output:
            handle_file()


main()
