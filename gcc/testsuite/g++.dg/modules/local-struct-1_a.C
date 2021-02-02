// { dg-additional-options -fmodules-ts }
export module foo;
// { dg-module-cmi foo }

export inline int __inline_signbitf (float __x)
{
  union x { float __f; unsigned int __u; } __u;

  __u.__f = __x;

  return int (__u.__u >> 31);
}
