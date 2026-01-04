/* Configuration common to all targets running Picolibc.
   Copyright (C) 2026 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#define PICOLIBC_LD "picolibc.ld"

/* Default to local-exec TLS model.  */
#undef OS_CC1_SPEC
#define OS_CC1_SPEC " %{!ftls-model=*:-ftls-model=local-exec}"

/* Pass along preprocessor definitions when --printf or --scanf are specified */
#define LIBC_CPP_SPEC				\
  " %{-printf=*: -D_PICOLIBC_PRINTF='%*'}"	\
  " %{-scanf=*: -D_PICOLIBC_SCANF='%*'}"

/*
 * Add picolibc.ld if not using -shared, -r or -T and we can find it.
 * Define vfprintf if --printf is set
 * Define vfscanf if --scanf is set
 */
#define LIBC_LINK_SPEC							\
  " %{!shared:%{!r:%{!T*: %:if-exists-then-else(%:find-file(" PICOLIBC_LD ") -T" PICOLIBC_LD ")}}}" \
  " %{-printf=*:--defsym=" USER_LABEL_PREFIX "vfprintf=" USER_LABEL_PREFIX "__%*_vfprintf}" \
  " %{-scanf=*:--defsym=" USER_LABEL_PREFIX "vfscanf=" USER_LABEL_PREFIX "__%*_vfscanf}"

/*
 * Place the C library, libgcc and any oslib in a link group to resolve
 * interdependencies
 */
#undef LIB_SPEC
#define LIB_SPEC "--start-group -lc %{-oslib=*:-l%*} %(libgcc) --end-group"

/* Select alternate crt0 version if --crt0 is specified */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%{-crt0=*:crt0-%*%O%s; :crt0%O%s}"

#define EH_TABLES_CAN_BE_READ_ONLY 1
