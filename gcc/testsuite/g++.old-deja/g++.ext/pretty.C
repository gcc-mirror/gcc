// Test for PRETTY_FUNCTION
// Build don't link:

class SV;

class SVTable // : public Debug
{
  // It is an array to pointer to a SV.
  SV ** array;

  // This is the size of array.
  int maxCount; 

  // This is the current element count.
  int count;

  void fatal_error (const char *f, ...);

public:
  SVTable (int size, const char *d);
  SVTable ();
  SVTable (const SVTable &);
  ~SVTable () {}

};


SVTable::SVTable (int size, const char *d)
	: maxCount (size), count (0)// , Debug (d)
{
  if (size < 0)
  {
    fatal_error ("%s: Invalid size: %d\n", __PRETTY_FUNCTION__, size);
  }

  array = (SV **) new SV * [size];

  if (array == 0)
  {
    fatal_error ("%s: Failed to allocate array\n", __PRETTY_FUNCTION__);
  }
}
