#!/usr/bin/env python3

# Python bindings for libgdiagnostics, using ctypes

# Contributed by David Malcolm <dmalcolm@redhat.com>
#
# Copyright (C) 2025 Free Software Foundation, Inc.
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
# along with GCC; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

from contextlib import contextmanager
import ctypes

# Lower-level API: use ctypes and FFI to wrap the C API directly

cdll = ctypes.cdll.LoadLibrary('libgdiagnostics.so')

libc = ctypes.CDLL(None)
c_stderr = ctypes.c_void_p.in_dll(libc, 'stderr')

# Opaque structs

class c_diagnostic_manager(ctypes.Structure):
    pass
c_diagnostic_manager_ptr = ctypes.POINTER(c_diagnostic_manager)

class c_diagnostic(ctypes.Structure):
    pass
c_diagnostic_ptr = ctypes.POINTER(c_diagnostic)

class c_diagnostic_file(ctypes.Structure):
    pass
c_diagnostic_file_ptr = ctypes.POINTER(c_diagnostic_file)

class c_diagnostic_physical_location(ctypes.Structure):
    pass
c_diagnostic_physical_location_ptr = ctypes.POINTER(c_diagnostic_physical_location)

# Enums

DIAGNOSTIC_COLORIZE_IF_TTY = 0

DIAGNOSTIC_LEVEL_ERROR   = 0
DIAGNOSTIC_LEVEL_WARNING = 1
DIAGNOSTIC_LEVEL_NOTE    = 2
DIAGNOSTIC_LEVEL_SORRY   = 3

# Entrypoints

cdll.diagnostic_manager_new.argtypes = []
cdll.diagnostic_manager_new.restype = c_diagnostic_manager_ptr

cdll.diagnostic_manager_release.argtypes = [c_diagnostic_manager_ptr]
cdll.diagnostic_manager_release.restype = None

cdll.diagnostic_manager_set_tool_name.argtypes = [c_diagnostic_manager_ptr,
                                                  ctypes.c_char_p]
cdll.diagnostic_manager_set_tool_name.restype = None

cdll.diagnostic_manager_begin_group.argtypes = [c_diagnostic_manager_ptr]
cdll.diagnostic_manager_begin_group.restype = None

cdll.diagnostic_manager_end_group.argtypes = [c_diagnostic_manager_ptr]
cdll.diagnostic_manager_end_group.restype = None

cdll.diagnostic_begin.argtypes = [c_diagnostic_manager_ptr,
                                  ctypes.c_int]
cdll.diagnostic_begin.restype = c_diagnostic_ptr

cdll.diagnostic_finish.argtypes = [c_diagnostic_ptr,
                                   ctypes.c_char_p]#, ctypes.c_char_p] # FIXME: should be variadic
cdll.diagnostic_finish.restype = None

cdll.diagnostic_manager_new_file.argtypes = [c_diagnostic_manager_ptr,
                                             ctypes.c_char_p,
                                             ctypes.c_char_p]
cdll.diagnostic_manager_new_file.restype = c_diagnostic_file_ptr

cdll.diagnostic_manager_new_location_from_file_and_line.argtypes \
    = [c_diagnostic_manager_ptr,
       c_diagnostic_file_ptr,
       ctypes.c_int]
cdll.diagnostic_manager_new_location_from_file_and_line.restype \
    = c_diagnostic_physical_location_ptr

cdll.diagnostic_manager_new_location_from_file_line_column.argtypes \
    = [c_diagnostic_manager_ptr,
       c_diagnostic_file_ptr,
       ctypes.c_int,
       ctypes.c_int]
cdll.diagnostic_manager_new_location_from_file_line_column.restype \
    = c_diagnostic_physical_location_ptr

cdll.diagnostic_manager_new_location_from_range.argtypes\
    = [c_diagnostic_manager_ptr,
       c_diagnostic_physical_location_ptr,
       c_diagnostic_physical_location_ptr,
       c_diagnostic_physical_location_ptr]
cdll.diagnostic_manager_new_location_from_range.restype \
    = c_diagnostic_physical_location_ptr

cdll.diagnostic_set_location.argtypes = [c_diagnostic_ptr,
                                         c_diagnostic_physical_location_ptr]
cdll.diagnostic_set_location.restype = None

cdll.diagnostic_add_fix_it_hint_replace.argtypes \
    = [c_diagnostic_ptr,
       c_diagnostic_physical_location_ptr,
       ctypes.c_char_p]
cdll.diagnostic_add_fix_it_hint_replace.restype = None

# Helper functions

def _to_utf8(s: str):
    if s is None:
        return None
    return s.encode('utf-8')

# Higher-level API, a more pythonic approach, with classes

class Manager:
    def __init__(self):
        self.c_mgr = cdll.diagnostic_manager_new()
        if 0:
            print('__init__: %r' % self.c_mgr)

    def __del__(self):
        if 0:
            print('__del__: %r' % self.c_mgr)
        self.c_mgr = cdll.diagnostic_manager_release(self.c_mgr)

    def set_tool_name(self, name: str):
        assert self.c_mgr
        assert str
        cdll.diagnostic_manager_set_tool_name (self.c_mgr, _to_utf8(name))

    def add_text_sink(self):
        assert self.c_mgr
        # FIXME: hardcode the args for now
        cdll.diagnostic_manager_add_text_sink (self.c_mgr,
                                               c_stderr,
                                               DIAGNOSTIC_COLORIZE_IF_TTY)

    def get_file(self, path: str, sarif_lang: str = None):
        assert self.c_mgr
        assert path
        c_file = cdll.diagnostic_manager_new_file (self.c_mgr,
                                                   _to_utf8(path),
                                                   _to_utf8(sarif_lang))
        return c_file

    def get_file_and_line(self,
                          c_file: c_diagnostic_file_ptr,
                          line_num: int):
        assert self.c_mgr
        assert c_file
        c_phys_loc = cdll.diagnostic_manager_new_location_from_file_and_line (self.c_mgr,
                                                                              c_file,
                                                                              line_num)
        return c_phys_loc

    def get_file_line_and_column(self,
                                 c_file: c_diagnostic_file_ptr,
                                 line_num: int,
                                 column_num: int):
        assert self.c_mgr
        assert c_file
        c_phys_loc = cdll.diagnostic_manager_new_location_from_file_line_column (self.c_mgr,
                                                                                 c_file,
                                                                                 line_num,
                                                                                 column_num)
        return c_phys_loc

    def get_range(self,
                  caret: c_diagnostic_physical_location_ptr,
                  start: c_diagnostic_physical_location_ptr,
                  finish: c_diagnostic_physical_location_ptr):
        assert self.c_mgr
        c_phys_loc = cdll.diagnostic_manager_new_location_from_range (self.c_mgr,
                                                                      caret,
                                                                      start,
                                                                      finish)
        return c_phys_loc

    @contextmanager
    def make_group(self):
        assert self.c_mgr
        cdll.diagnostic_manager_begin_group (self.c_mgr)
        try:
            yield
        finally:
            assert self.c_mgr
            cdll.diagnostic_manager_end_group (self.c_mgr)

class Diagnostic:
    def __init__(self, mgr: Manager, level: int):
        self.c_diag = cdll.diagnostic_begin (mgr.c_mgr, level)
        if 0:
            print('__init__: %r' % self.c_diag)

    def finish(self, fmt: str, *args):
        assert self.c_diag
        c_args = []
        for arg in args:
            if type(arg) is str:
                arg = _to_utf8(arg)
            c_args.append(arg)
        cdll.diagnostic_finish (self.c_diag, _to_utf8(fmt), *c_args)
        self.c_diagnostic = None

    def set_location(self,
                     phys_loc: c_diagnostic_physical_location_ptr):
        assert self.c_diag
        cdll.diagnostic_set_location(self.c_diag, phys_loc)


    # Chaining-style API

    def at(self,
           phys_loc: c_diagnostic_physical_location_ptr):
        self.set_location(phys_loc)
        return self

    def with_fix_it_replace(self,
                            phys_loc: c_diagnostic_physical_location_ptr,
                            replacement: str):
        assert self.c_diag
        assert replacement
        cdll.diagnostic_add_fix_it_hint_replace(self.c_diag,
                                                phys_loc,
                                                _to_utf8(replacement))
        return self

    def emit(self, fmt: str, *args):
        self.finish(fmt, *args)
