void
f1 (o1, o2, o3, i, j, k)
     long long *o1, *o2, *o3;
     int i, j, k;
{
  while (--i)
    o1[i] = o2[j >>= 1] + o3[k >>= 1];
}

void
f2 (o1, o2, o3, i, j, k)
     long long *o1, *o2, *o3;
     int i, j, k;
{
  while (--i)
    o1[i] = o2[j >>= 1] - o3[k >>= 1];
}

void
f3 (o1, o2, o3, i, j, k)
     long long *o1, *o3;
     unsigned *o2;
     int i, j, k;
{
  while (--i)
    o1[i] = o2[j >>= 1] + o3[k >>= 1];
}

void
f4 (o1, o2, o3, i, j, k)
     long long *o1, *o2;
     unsigned *o3;
     int i, j, k;
{
  while (--i)
    o1[i] = o2[j >>= 1] - o3[k >>= 1];
}
