/* File caching.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.

This file is part of GCC.

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

#ifndef GCC_LTO_LTRANS_CACHE_H
#define GCC_LTO_LTRANS_CACHE_H

#include "lockfile.h"

using checksum_t = std::array<uint8_t, 32>;

class ltrans_file_cache
{
public:
  /* Cache item representing input/output filename pair.  */
  struct item
  {
    item (std::string input, std::string output,
	  checksum_t input_checksum, uint32_t last_used);
    ~item ();

    /* Full path to input filename.  */
    const std::string input;
    /* Full path to output filename.  */
    const std::string output;
    /* Checksum of input file.  */
    const checksum_t input_checksum;

    /* Larger last_used corresponds to later usage.  */
    uint32_t last_used;

    /* Lockfile so that output file can be created later than input file.  */
    lockfile lock;
  };

  /* Constructor.  Resulting cache item filenames will be
     in format `prefix%d[.ltrans]suffix`.  */
  ltrans_file_cache (const char* dir, const char* prefix, const char* suffix,
		     size_t soft_cache_size);
  /* Destructor.  */
  ~ltrans_file_cache ();

  /* Loads data about previously cached items from cachedata file.

     Must be called with creation_lock or deletion_lock held to
     prevent data race.  */
  void load_cache ();

  /* Rewrites data about cache items into cachedata file.

     Must be only called when creation_lock or deletion_lock was held since last
     call to load_cache.  */
  void save_cache ();


  /* Adds input file into cache.  Cache item with input file identical to
     added input file will be returned as _item.
     If the file was already cached, `true` is returned, `false` otherwise.
     The added input file is deleted (or moved).

     Must be called with creation_lock held to prevent data race.  */
  bool add_to_cache (const char* filename, item*& _item);

  /* If exists, returns cache item corresponding to cached input file.  */
  item* get_item (const char* input);

  /* If no other process holds the deletion_lock, prunes oldest unused cache
     items over limit.  */
  void try_prune ();

  /* Clears cache class, as if only constructor was called.  */
  void cleanup ();

  /* Cache is enabled if true.  */
  operator bool ()
  {
    return dir;
  }

  /* Access to already created items can be concurrent with item creation.  */
  lockfile creation_lock;
  /* Access to already created items cannot be concurrent
     with item deletion.  */
  lockfile deletion_lock;

  /* Directory of cache.  NULL if cache is disabled.  */
  const char* dir;
private:
  /* Adds given cache item to all relevant datastructures.  */
  void add_item (item* item);

  /* Creates new cache item with given checksum.
     New input/output files are chosen to not collide with other items.

     Must be called with creation_lock held to prevent data race.  */
  item* create_item (const checksum_t& checksum);

  /* Prunes oldest unused cache items over limit.
     Must be called with deletion_lock held to prevent data race.  */
  void prune ();

  /* Creates cachedata filename for save/load.  */
  std::string filename_cachedata ();

  /* All cache items in current cache.  */
  std::vector<item*> items;
  std::map<checksum_t, item*> map_checksum;
  std::map<std::string, item*> map_input;

  /* Cached filenames are in format "prefix%d[.ltrans]suffix".  */
  const char* prefix;
  const char* suffix;

  /* If cache items count is larger, prune deletes old items.  */
  size_t soft_cache_size;

  /* Counter used to populate last_used of items.  */
  uint32_t usage_counter;

  /* String in format "dir/prefix".  */
  std::string cache_prefix;
  /* Lower indices are occupied.  */
  uint32_t cache_free_idx;

  /* Buffer for sprintf.  */
  char* str_buffer;
};

#endif
