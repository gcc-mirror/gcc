void foo(){
  __builtin___clear_cache(0, 0);
}

void foo1(){
  __builtin___clear_cache((void*)0, (void*)0);
}

void foo2(){
  void *yy = 0;
  __builtin___clear_cache(yy, yy);
}

void foo3(){
  void *yy = (void*)0x1000;
  __builtin___clear_cache(yy, yy);
}

