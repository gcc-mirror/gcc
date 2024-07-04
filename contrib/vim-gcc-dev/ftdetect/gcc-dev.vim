" Vim file type detection rules for GCC development
"
" Copyright (C) 2018-2024 Free Software Foundation, Inc.
"
" This script is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 3, or (at your option)
" any later version

augroup filetypedetect

  au BufRead match.pd                setf gcc-match

  " Match RTL dump file names such as test.c.234r.pass-name
  au BufRead *.[1-3][0-9][0-9]r.*    setf gcc-rtl

  " Match GIMPLE and IPA dump file names
  au BufRead *.[0-2][0-9][0-9][ti].* setf gimple

augroup END
