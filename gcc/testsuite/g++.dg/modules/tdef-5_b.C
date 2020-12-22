// { dg-additional-options -fmodules-ts }
module foo;

_IO_lock_t bob ()
{
  return _lock;
}
