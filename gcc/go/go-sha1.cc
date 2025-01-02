/* go-sha1.cc -- Go frontend interface to gcc backend.
   Copyright (C) 2016-2025 Free Software Foundation, Inc.

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

#include "go-sha1.h"
#include "sha1.h"

class Gcc_sha1_helper : public Go_sha1_helper
{
 public:

  Gcc_sha1_helper() : ctx_(new sha1_ctx) { sha1_init_ctx(this->ctx_); }

  ~Gcc_sha1_helper();

  // Incorporate 'len' bytes from 'buffer' into checksum.
  void
  process_bytes(const void* buffer, size_t len);

  // Finalize checksum and return in the form of a string.
  std::string
  finish();

 private:
  sha1_ctx *ctx_;
};

Gcc_sha1_helper::~Gcc_sha1_helper()
{
  delete ctx_;
}

void
Gcc_sha1_helper::process_bytes(const void* buffer, size_t len)
{
  sha1_process_bytes(buffer, len, this->ctx_);
}

std::string
Gcc_sha1_helper::finish()
{
  // Use a union to provide the required alignment.
  union
  {
    char checksum[checksum_len];
    long align;
  } u;
  sha1_finish_ctx(this->ctx_, u.checksum);
  return std::string(u.checksum, checksum_len);
}

Go_sha1_helper*
go_create_sha1_helper()
{
  return new Gcc_sha1_helper();
}
