/* ChanConsts.h define enum to be used by C components.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* These must match ChanConsts.def.   */

typedef enum openResults {
  opened,           /* the open succeeded as requested */
  wrongNameFormat,  /* given name is in the wrong format for the implementation */
  wrongFlags,       /* given flags include a value that does not apply to the device */
  tooManyOpen,      /* this device cannot support any more open channels */
  outOfChans,       /* no more channels can be allocated */
  wrongPermissions, /* file or directory permissions do not allow request */
  noRoomOnDevice,   /* storage limits on the device prevent the open */
  noSuchFile,       /* a needed file does not exist */
  fileExists,       /* a file of the given name already exists when a new one is required */
  wrongFileType,    /* the file is of the wrong type to support the required operations */
  noTextOperations, /* text operations have been requested, but are not supported */
  noRawOperations,  /* raw operations have been requested, but are not supported */
  noMixedOperations,/* text and raw operations have been requested, but they
		       are not supported in combination */
  alreadyOpen,      /* the source/destination is already open for operations not supported
		       in combination with the requested operations */
  otherProblem      /* open failed for some other reason */
} openResults;
