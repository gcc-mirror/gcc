/* { dg-options "-msve-vector-bits=256 -W -Wall" } */

#include <arm_sve.h>

#define N __ARM_FEATURE_SVE_BITS
#define FIXED_ATTR __attribute__ ((arm_sve_vector_bits (N)))

template<typename T> struct foo { T var; };

typedef svint8_t var1;
typedef __SVInt8_t var2;

typedef const var1 const_var1;
typedef const var2 const_var2;

typedef svint8_t fixed1 FIXED_ATTR;
typedef svint8_t fixed1_alias FIXED_ATTR;
typedef __SVInt8_t fixed2 FIXED_ATTR;

typedef const_var1 const_fixed1 FIXED_ATTR;
typedef const var1 const_fixed1_alias FIXED_ATTR;
typedef const_var2 const_fixed2 FIXED_ATTR;

extern fixed1 extern1;
extern fixed1_alias extern1_alias;
extern fixed2 extern2;

extern foo<fixed1> extern1_foo;
extern foo<fixed1_alias> extern1_alias_foo;
extern foo<fixed2> extern2_foo;

extern const_fixed1 const_extern1;
extern const_fixed1_alias const_extern1_alias;
extern const_fixed2 const_extern2;

extern foo<const_fixed1> const_extern1_foo;
extern foo<const_fixed1_alias> const_extern1_alias_foo;
extern foo<const_fixed2> const_extern2_foo;

fixed1 &ref1a = extern1;
fixed1_alias &ref1b = extern1;
fixed2 &ref1c = extern1;

fixed1 &ref2a = extern1_alias;
fixed1_alias &ref2b = extern1_alias;
fixed2 &ref2c = extern1_alias;

fixed1 &ref3a = extern2;
fixed1_alias &ref3b = extern2;
fixed2 &ref3c = extern2;

fixed1 &ref1a_foo = extern1_foo.var;
fixed1_alias &ref1b_foo = extern1_foo.var;
fixed2 &ref1c_foo = extern1_foo.var;

fixed1 &ref2a_foo = extern1_alias_foo.var;
fixed1_alias &ref2b_foo = extern1_alias_foo.var;
fixed2 &ref2c_foo = extern1_alias_foo.var;

fixed1 &ref3a_foo = extern2_foo.var;
fixed1_alias &ref3b_foo = extern2_foo.var;
fixed2 &ref3c_foo = extern2_foo.var;

fixed1 &ref4a = const_extern1; 	     // { dg-error {discards qualifiers} }
fixed1_alias &ref4b = const_extern1; // { dg-error {discards qualifiers} }
fixed2 &ref4c = const_extern1;       // { dg-error {discards qualifiers} }

fixed1 &ref4a_foo = const_extern1_foo.var; 	     // { dg-error {discards qualifiers} }
fixed1_alias &ref4b_foo = const_extern1_foo.var; // { dg-error {discards qualifiers} }
fixed2 &ref4c_foo = const_extern1_foo.var;       // { dg-error {discards qualifiers} }

const fixed1 &ref5a = const_extern2;
const fixed1_alias &ref5b = const_extern2;
const fixed2 &ref5c = const_extern2;

const_fixed1 &const_ref1a = extern1;
const_fixed1_alias &const_ref1b = extern1;
const_fixed2 &const_ref1c = extern1;

const_fixed1 &const_ref2a = extern1_alias;
const_fixed1_alias &const_ref2b = extern1_alias;
const_fixed2 &const_ref2c = extern1_alias;

const_fixed1 &const_ref3a = extern2;
const_fixed1_alias &const_ref3b = extern2;
const_fixed2 &const_ref3c = extern2;

const_fixed1 &const_ref1a_foo = extern1_foo.var;
const_fixed1_alias &const_ref1b_foo = extern1_foo.var;
const_fixed2 &const_ref1c_foo = extern1_foo.var;

const_fixed1 &const_ref2a_foo = extern1_alias_foo.var;
const_fixed1_alias &const_ref2b_foo = extern1_alias_foo.var;
const_fixed2 &const_ref2c_foo = extern1_alias_foo.var;

const_fixed1 &const_ref3a_foo = extern2_foo.var;
const_fixed1_alias &const_ref3b_foo = extern2_foo.var;
const_fixed2 &const_ref3c_foo = extern2_foo.var;

const_fixed1 &const_ref4a = const_extern1;
const_fixed1_alias &const_ref4b = const_extern1;
const_fixed2 &const_ref4c = const_extern1;

const_fixed1 &const_ref5a = const_extern1_alias;
const_fixed1_alias &const_ref5b = const_extern1_alias;
const_fixed2 &const_ref5c = const_extern1_alias;

const_fixed1 &const_ref6a = const_extern2;
const_fixed1_alias &const_ref6b = const_extern2;
const_fixed2 &const_ref6c = const_extern2;

const_fixed1 &const_ref4a_foo = const_extern1_foo.var;
const_fixed1_alias &const_ref4b_foo = const_extern1_foo.var;
const_fixed2 &const_ref4c_foo = const_extern1_foo.var;

const_fixed1 &const_ref5a_foo = const_extern1_alias_foo.var;
const_fixed1_alias &const_ref5b_foo = const_extern1_alias_foo.var;
const_fixed2 &const_ref5c_foo = const_extern1_alias_foo.var;

const_fixed1 &const_ref6a_foo = const_extern2_foo.var;
const_fixed1_alias &const_ref6b_foo = const_extern2_foo.var;
const_fixed2 &const_ref6c_foo = const_extern2_foo.var;
