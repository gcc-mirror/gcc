/* Test if #line commands are generated properly even when header
   includes self.  */
#include "20000628-1a.h"
#ifndef t20000628_1_h
#define t20000628_1_h 1
#include "20000628-1.h"
#include "20000628-1a.h"
#endif
