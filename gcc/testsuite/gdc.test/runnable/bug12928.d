// PERMUTE_ARGS: -inline -g -O
import core.exception : RangeError;
void main(string[] args)
{
  int[2] a;
  try
  {
    foreach(const i; 0..3)
      a[i] = i;
    assert(0);
  }
  catch(RangeError){}
}
