// { dg-do assemble  }
// prms-id: 7476

class HeapTracked {
public:
  virtual ~HeapTracked() { }
  static void isObjectAllocation(const HeapTracked *ptr);
  static void isObjectAllocation(HeapTracked *ptr);
};

void HeapTracked::isObjectAllocation(HeapTracked *ptr)
{
  dynamic_cast<const void*>(ptr);
  dynamic_cast<void*>(ptr);
}
void HeapTracked::isObjectAllocation(const HeapTracked *ptr)
{
  const_cast<void*>(dynamic_cast<const void*>(ptr));
  dynamic_cast<void*>(ptr);		// { dg-error "" } 
}
