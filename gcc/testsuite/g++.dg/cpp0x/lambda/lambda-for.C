// PR c++/50114
// { dg-do compile { target c++11 } }
// { dg-options "-w" }

int open()
{
  int *x2feed_i = 0;
  auto insert_feed = [&](unsigned char venue, int* newfeed)
  {
     for(int x2feed_i = 1; 0; ) ;
     x2feed_i = newfeed;
  };
}
