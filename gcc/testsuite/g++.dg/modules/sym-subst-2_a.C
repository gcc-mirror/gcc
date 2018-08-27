// For some reason -ffat-lto-objects gets added to the _b options.  I
// have no idea why.
// { dg-additional-options -ffat-lto-objects }
export module bob.stuart;
// { dg-module-bmi bob.stuart }

template <typename T> void inner (T &)
{
}

export template <typename T> void foo (T &x)
{
  inner (x);
}

