// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T> void test(){
  try {
  }
  catch(int x){
  }
}

template void test<int>();
