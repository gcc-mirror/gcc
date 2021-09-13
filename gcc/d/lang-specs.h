/* lang-specs.h -- GCC driver specs for D frontend.
   Copyright (C) 2006-2021 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This is the contribution to the `default_compilers' array in gcc.c
   for the D language.  */

{".d", "@d", 0, 1, 0 },
{".dd", "@d", 0, 1, 0 },
{".di", "@d", 0, 1, 0 },
{"@d",
  "%{!E:d21 %i %(cc1_options) %I %{nostdinc*} %{i*} %{I*} %{J*} \
    %{H} %{Hd*} %{Hf*} %{MD:-MD %b.deps} %{MMD:-MMD %b.deps} \
    %{M} %{MM} %{MF*} %{MG} %{MP} %{MQ*} %{MT*} \
    %{X:-Xf %b.json} %{Xf*} \
    %{v} %{!fsyntax-only:%(invoke_as)}}", 0, 1, 0 },
