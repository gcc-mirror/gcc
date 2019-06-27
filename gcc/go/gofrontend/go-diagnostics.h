// go-diagnostics.h -- interface to diagnostic reporting   -*- C++ -*-

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_DIAGNOSTICS_H
#define GO_DIAGNOSTICS_H

#include "go-linemap.h"

#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1)
#define GO_ATTRIBUTE_GCC_DIAG(m, n) __attribute__ ((__format__ (__gcc_tdiag__, m, n))) __attribute__ ((__nonnull__ (m)))
#else
#define GO_ATTRIBUTE_GCC_DIAG(m,  n)
#endif

#if __GNUC__ >= 3
#define GO_ATTRIBUTE_PRINTF(m, n) __attribute__ ((__format__ (__printf__, m, n))) __attribute__ ((__nonnull__ (m)))
#else
#define GO_ATTRIBUTE_PRINTF(m, n)
#endif

// These declarations define the interface through which the frontend
// reports errors and warnings. These functions accept printf-like
// format specifiers (e.g. %d, %f, %s, etc), with the following additional
// extensions:
//
//  1.  'q' qualifier may be applied to a specifier to add quoting, e.g.
//      %qd produces a quoted decimal output, %qs a quoted string output.
//      [This extension is supported only with single-character format
//      specifiers].
//
//  2.  %m specifier outputs value of "strerror(errno)" at time of call.
//
//  3.  %< outputs an opening quote, %> a closing quote.
//
// All other format specifiers are as defined by 'sprintf'. The final resulting
// message is then sent to the back end via go_be_error_at/go_be_warning_at.

extern void go_error_at(const Location, const char* fmt, ...)
    GO_ATTRIBUTE_GCC_DIAG(2,3);
extern void go_warning_at(const Location, int opt, const char* fmt, ...)
    GO_ATTRIBUTE_GCC_DIAG(3,4);
extern void go_fatal_error(const Location, const char* fmt, ...)
    GO_ATTRIBUTE_GCC_DIAG(2,3);
extern void go_inform(const Location, const char* fmt, ...)
    GO_ATTRIBUTE_GCC_DIAG(2,3);

// go_debug is used to report a debugging message at a location.  This
// uses standard printf formatting.

extern void go_debug(const Location, const char* fmt, ...)
  GO_ATTRIBUTE_PRINTF(2, 3);

// These interfaces provide a way for the front end to ask for
// the open/close quote characters it should use when formatting
// diagnostics (warnings, errors).
extern const char* go_open_quote();
extern const char* go_close_quote();

// These interfaces are used by utilities above to pass warnings and
// errors (once format specifiers have been expanded) to the back end,
// and to determine quoting style. Avoid calling these routines directly;
// instead use the equivalent routines above. The back end is required to
// implement these routines.

extern void go_be_error_at(const Location, const std::string& errmsg);
extern void go_be_warning_at(const Location, int opt,
                             const std::string& warningmsg);
extern void go_be_fatal_error(const Location, const std::string& errmsg);
extern void go_be_inform(const Location, const std::string& infomsg);
extern void go_be_get_quotechars(const char** open_quote,
                                 const char** close_quote);

#endif // !defined(GO_DIAGNOSTICS_H)
