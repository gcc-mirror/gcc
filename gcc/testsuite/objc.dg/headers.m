// Test for obscure conflicts with the system headers (inspired by similar
// test in libstdc++-v3).  Author: Loren J. Rittle <ljrittle@acm.org>.
// { dg-options "-Wall -Wpointer-arith -Wcast-qual -Wstrict-prototypes -Wshadow" }
// { dg-do compile }

#ifdef __NEXT_RUNTIME__
#include <Foundation/NSString.h>
#else
#include <objc/NXConstStr.h>
#endif
#include <objc/Object.h>
#include <objc/Protocol.h>
#ifdef __NEXT_RUNTIME__
#include <objc/objc-runtime.h>
#else
#include <objc/encoding.h>
#include <objc/hash.h>
#endif

#include <objc/objc-api.h>
#ifndef __NEXT_RUNTIME__
#include <objc/objc-list.h>
#endif

#include <objc/objc.h>

#ifndef __NEXT_RUNTIME__
#include <objc/sarray.h>
#include <objc/thr.h>
#include <objc/typedstream.h>
#endif
