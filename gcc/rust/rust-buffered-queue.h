// Copyright (C) 2020-2023 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef RUST_BUFFERED_QUEUE_H
#define RUST_BUFFERED_QUEUE_H

#include "rust-system.h"

namespace Rust {
/* Buffered queue implementation. Items are of type T, queue source is of type
 * Source. Note that this is owning of the source. */
template <typename T, typename Source> class buffered_queue
{
public:
  // Construct empty queue from Source src.
  buffered_queue (Source src) : source (src), start (0), end (0), buffer () {}

  /* disable copying (since source is probably non-copyable)
   * TODO is this actually a good idea? If source is non-copyable, it would
   * just delete the copy constructor anyway.*/
  buffered_queue (const buffered_queue &other) = delete;
  buffered_queue &operator= (const buffered_queue &other) = delete;

  // enable moving
  buffered_queue (buffered_queue &&other) = default;
  buffered_queue &operator= (buffered_queue &&other) = default;

  // Returns token at position start + n (i.e. n tokens ahead).
  T peek (int n)
  {
    // n should not be behind
    rust_assert (n >= 0);

    int num_queued_items = end - start;
    int num_items_required = n + 1;

    // if required items go past end of queue, add them to queue
    if (num_items_required > num_queued_items)
      {
	int num_items_to_read = num_items_required - num_queued_items;

	/* if queue length + extra items is larger than buffer size, expand
	 * buffer */
	if (end + num_items_to_read > (int) buffer.size ())
	  {
	    // Resize the buffer by 1.5x
	    int new_size = (buffer.size () + num_items_to_read);
	    new_size += (new_size >> 1);

	    // old method:
	    /*
		  // create new queue buffer with new size
		  std::vector<T> new_queue (new_size);
		  std::copy (buffer.begin () + start, buffer.begin () + end,
			     new_queue.begin ());
		  start = 0;
		  end = num_queued_items;
		  // TODO: would move be better here? optimisation for move with
		  // shared pointer?

		  // swap member buffer and new queue buffer
		  std::swap (buffer, new_queue);
	    */

	    // TODO: determine overhead of this approach vs copy. Should be
	    // lower.
	    std::vector<T> new_queue;
	    new_queue.reserve (new_size);
	    new_queue.insert (new_queue.begin (),
			      std::make_move_iterator (buffer.begin () + start),
			      std::make_move_iterator (buffer.begin () + end));
	    start = 0;
	    end = num_queued_items;
	    // fill up rest of vector with junk so that indexing can work
	    new_queue.insert (new_queue.begin () + end,
			      new_size - new_queue.size (), T ());

	    buffer = std::move (new_queue);
	    /* this should be best method - std::move(range) would have
	     * allocation problems; initial construction would require
	     * reallocation upon resizing */

	    // validate that buffer is large enough now
	    rust_assert (end + num_items_to_read <= (int) buffer.size ());
	  }

	/* iterate through buffer and invoke operator () on source on values
	 * past original end */
	for (int i = 0; i < num_items_to_read; i++)
	  buffer[end + i] = source.next ();

	// move end based on additional items added
	end += num_items_to_read;
      }

    rust_assert (0 <= start);
    rust_assert (start <= end);
    rust_assert (end <= (int) buffer.size ());

    rust_assert (start + n < end);

    // return value at start + n in buffer
    return buffer[start + n];
  }

  /* TODO: add faster peek current token to remove overhead of conditional
   * branches? */

  // Advances start by n + 1.
  void skip (int n)
  {
    // Call peek to ensure requested n is actually in queue.
    peek (n);

    // Clear queue values from start to n (inclusive).
    for (int i = 0; i < (n + 1); i++)
      buffer[start + i] = T ();

    // Move start forward by n + 1.
    start += (n + 1);

    // Ensure start is not impossible somehow
    rust_assert (0 <= start);
    rust_assert (start <= end);

    // Compact buffer if empty
    if (start == end)
      start = end = 0;
  }

  /* Inserts element at front of vector. Really dirty hack with terrible
   * performance, only use when really needed. */
  void insert_at_front (T elem_to_insert)
  {
    // TODO: test as this may not work properly

    // Insert actual element in buffer at start.
    buffer.insert (buffer.begin (), elem_to_insert);

    /* Increase the end number since added element means all others have shifted
     * one along */
    end++;
  }

  // Insert at arbitrary position (attempt)
  void insert (int index, T elem_to_insert)
  {
    // TODO: test as this may not work properly

    // n should not be behind
    rust_assert (index >= 0);

    // call peek to ensure that the items behind this (at least) are in queue
    if (index >= 1)
      peek (index - 1);
    else
      peek (index);

    buffer.insert (buffer.begin () + start + index, std::move (elem_to_insert));

    end++;
  }

  // Replaces the current value in the buffer. Total HACK.
  void replace_current_value (T replacement)
  {
    // call peek to ensure value exists
    peek (0);

    buffer[start] = std::move (replacement);

    // don't move start or end
  }

private:
  // Source of tokens for queue.
  Source source;

  // Begin of range in buffer, inclusive.
  int start;
  // End of range in buffer, exclusive.
  int end;

  // Queue buffer.
  std::vector<T> buffer;
};
} // namespace Rust

#endif
