/* Definitions for option handling for SPARC.
   Copyright (C) 1996, 1999, 2002, 2006, 2007, 2010, 2011
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef SPARC_OPTS_H
#define SPARC_OPTS_H

/* Processor type.
   These must match the values for the cpu attribute in sparc.md and
   the table in sparc_option_override.  */
enum processor_type {
  PROCESSOR_V7,
  PROCESSOR_CYPRESS,
  PROCESSOR_V8,
  PROCESSOR_SUPERSPARC,
  PROCESSOR_HYPERSPARC,
  PROCESSOR_LEON,
  PROCESSOR_SPARCLITE,
  PROCESSOR_F930,
  PROCESSOR_F934,
  PROCESSOR_SPARCLITE86X,
  PROCESSOR_SPARCLET,
  PROCESSOR_TSC701,
  PROCESSOR_V9,
  PROCESSOR_ULTRASPARC,
  PROCESSOR_ULTRASPARC3,
  PROCESSOR_NIAGARA,
  PROCESSOR_NIAGARA2,
  PROCESSOR_NIAGARA3,
  PROCESSOR_NIAGARA4,
  PROCESSOR_NATIVE
};

/* Sparc system memory model.  See Appendix D in the Sparc V9 manual
   for formal specification, and Appendix J for more discussion.  */
enum sparc_memory_model_type {
  SMM_DEFAULT,	/* Uninitialized.  */
  SMM_RMO,	/* Relaxed Memory Order.  */
  SMM_PSO,	/* Partial Store Order.  */
  SMM_TSO,	/* Total Store Order.  */
  SMM_SC	/* Sequential Consistency.  */
};

#endif
