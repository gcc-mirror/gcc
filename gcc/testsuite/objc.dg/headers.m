// Test for obscure conflicts with the system headers (inspired by similar
// test in libstdc++-v3).  Author: Loren J. Rittle <ljrittle@acm.org>.
// { dg-options "-Wall -Wpointer-arith -Wcast-qual -Wstrict-prototypes -Wshadow" }
// { dg-do compile }

#include <objc/NXConstStr.h>
#include <objc/Object.h>
#include <objc/Protocol.h>
#include <objc/encoding.h>
#include <objc/hash.h>
#include <objc/objc-api.h>
#include <objc/objc-list.h>
#include <objc/objc.h>
#include <objc/sarray.h>
#include <objc/thr.h>
#include <objc/typedstream.h>
