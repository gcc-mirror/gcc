// PR c++/50114
// { dg-options "-std=c++0x -w" }

int open()
{
  int *x2feed_i = 0;
  auto insert_feed = [&](unsigned char venue, int* newfeed)
  {
     for(int x2feed_i = 1; 0; ) ;
     x2feed_i = newfeed;
  };
}
