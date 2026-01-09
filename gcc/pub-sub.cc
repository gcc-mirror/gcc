/* Loosely-coupled notifications via the Publish-Subscribe pattern.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#define INCLUDE_LIST
#define INCLUDE_STRING
#include "config.h"

#include "system.h"
#include "coretypes.h"

#include "pub-sub.h"


#if CHECKING_P

#include "selftest.h"

namespace selftest {

/* Selftests.  */

// A topic for use in selftests

namespace snafu {

struct paper_jam {};

struct out_of_paper
{
  int tray;
};

struct ink_low
{
  std::string color;
};

struct subscriber
{
  virtual void on_message (const paper_jam &m) = 0;
  virtual void on_message (const out_of_paper &m) = 0;
  virtual void on_message (const ink_low &m) = 0;
};

} // namespace snafu

static void
test_example ()
{
  struct logger : public snafu::subscriber
  {
    void on_message (const snafu::paper_jam &) final override
    {
      m_log += "paper jam\n";
    }
    void on_message (const snafu::out_of_paper &m) final override
    {
      m_log += "out of paper (tray " + std::to_string (m.tray) + ")\n";
    }
    void on_message (const snafu::ink_low &m) final override
    {
      m_log += "ink low: " + m.color + "\n";
    }

    std::string m_log;
  };

  pub_sub::channel<snafu::subscriber> printer_a;
  pub_sub::channel<snafu::subscriber> printer_b;
  pub_sub::channel<snafu::subscriber> printer_c;

  // No subscribers yet
  ASSERT_EQ (printer_a.get_if_active (), nullptr);
  ASSERT_EQ (printer_b.get_if_active (), nullptr);
  ASSERT_EQ (printer_c.get_if_active (), nullptr);

  // Subscribers to individual channels
  logger log_a;
  logger log_b;
  logger log_c;
  printer_a.add_subscriber (log_a);
  printer_b.add_subscriber (log_b);
  printer_c.add_subscriber (log_c);

  // A subscriber to all channels
  logger log_all;
  printer_a.add_subscriber (log_all);
  printer_b.add_subscriber (log_all);
  printer_c.add_subscriber (log_all);

  // The channels now have subscribers
  ASSERT_EQ (printer_a.get_if_active (), &printer_a);
  ASSERT_EQ (printer_b.get_if_active (), &printer_b);
  ASSERT_EQ (printer_c.get_if_active (), &printer_c);

  // Publish a message to each channel
  printer_a.publish (snafu::paper_jam {});
  printer_b.publish (snafu::out_of_paper {1});
  printer_c.publish (snafu::ink_low {"cyan"});

  // Verify that the subscribers got the messages they were meant to
  ASSERT_EQ (log_a.m_log, "paper jam\n");
  ASSERT_EQ (log_b.m_log, "out of paper (tray 1)\n");
  ASSERT_EQ (log_c.m_log, "ink low: cyan\n");
  ASSERT_EQ (log_all.m_log,
	     "paper jam\n"
	     "out of paper (tray 1)\n"
	     "ink low: cyan\n");
}

/* Run all of the selftests within this file.  */

void
pub_sub_cc_tests ()
{
  test_example ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
