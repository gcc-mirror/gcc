// PR C++/28906: new on an array causes incomplete arrays to
// become complete with the wrong size.

// the bounds of xvalue_store was being set to include want
// which was incorrect.

// { dg-do compile }

extern unsigned char xvalue_store[];
bool reserve (int want)
{
  new unsigned char[want];
  return true;
}
unsigned char xvalue_store[257];
