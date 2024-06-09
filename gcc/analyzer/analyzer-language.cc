/* Interface between analyzer and frontends.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "stringpool.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-language.h"
#include "analyzer/analyzer-logging.h"
#include "diagnostic.h"

/* Map from identifier to INTEGER_CST.  */
static GTY (()) hash_map <tree, tree> *analyzer_stashed_constants;

#if ENABLE_ANALYZER

namespace ana {
static vec<finish_translation_unit_callback>
    *finish_translation_unit_callbacks;

void
register_finish_translation_unit_callback (
    finish_translation_unit_callback callback)
{
  if (!finish_translation_unit_callbacks)
    vec_alloc (finish_translation_unit_callbacks, 1);
  finish_translation_unit_callbacks->safe_push (callback);
}

static void
run_callbacks (logger *logger, const translation_unit &tu)
{
  for (auto const &cb : finish_translation_unit_callbacks)
    {
      cb (logger, tu);
    }
}

/* Call into TU to try to find a value for NAME.
   If found, stash its value within analyzer_stashed_constants.  */

static void
maybe_stash_named_constant (logger *logger,
			    const translation_unit &tu,
			    const char *name)
{
  LOG_FUNC_1 (logger, "name: %qs", name);

  if (!analyzer_stashed_constants)
    analyzer_stashed_constants = hash_map<tree, tree>::create_ggc ();

  tree id = get_identifier (name);
  if (tree t = tu.lookup_constant_by_id (id))
    {
      gcc_assert (TREE_CODE (t) == INTEGER_CST);
      analyzer_stashed_constants->put (id, t);
      if (logger)
	logger->log ("%qs: %qE", name, t);
    }
  else
    {
      if (logger)
	logger->log ("%qs: not found", name);
    }
}

/* Call into TU to try to find values for the names we care about.
   If found, stash their values within analyzer_stashed_constants.  */

static void
stash_named_constants (logger *logger, const translation_unit &tu)
{
  LOG_SCOPE (logger);

  /* Stash named constants for use by sm-fd.cc  */
  maybe_stash_named_constant (logger, tu, "O_ACCMODE");
  maybe_stash_named_constant (logger, tu, "O_RDONLY");
  maybe_stash_named_constant (logger, tu, "O_WRONLY");
  maybe_stash_named_constant (logger, tu, "SOCK_STREAM");
  maybe_stash_named_constant (logger, tu, "SOCK_DGRAM");
}

/* Hook for frontend to call into analyzer when TU finishes.
   This exists so that the analyzer can stash named constant values from
   header files (e.g. macros and enums) for later use when modeling the
   behaviors of APIs.

   By doing it this way, the analyzer can use the precise values for those
   constants from the user's headers, rather than attempting to model them
   as properties of the target.  */

void
on_finish_translation_unit (const translation_unit &tu)
{
  /* Bail if the analyzer isn't enabled.  */
  if (!flag_analyzer)
    return;

  FILE *logfile = get_or_create_any_logfile ();
  log_user the_logger (NULL);
  if (logfile)
    the_logger.set_logger (new logger (logfile, 0, 0,
				       *global_dc->printer));
  stash_named_constants (the_logger.get_logger (), tu);

  run_callbacks (the_logger.get_logger (), tu);
}

/* Lookup NAME in the named constants stashed when the frontend TU finished.
   Return either an INTEGER_CST, or NULL_TREE.  */

tree
get_stashed_constant_by_name (const char *name)
{
  if (!analyzer_stashed_constants)
    return NULL_TREE;
  tree id = get_identifier (name);
  if (tree *slot = analyzer_stashed_constants->get (id))
    {
      gcc_assert (TREE_CODE (*slot) == INTEGER_CST);
      return *slot;
    }
  return NULL_TREE;
}

/* Log all stashed named constants to LOGGER.  */

void
log_stashed_constants (logger *logger)
{
  gcc_assert (logger);
  LOG_SCOPE (logger);
  if (analyzer_stashed_constants)
    for (auto iter : *analyzer_stashed_constants)
      logger->log ("%qE: %qE", iter.first, iter.second);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */

#include "gt-analyzer-language.h"
