void
f (void)
{
  unsigned char c, ca[15], caa[20][30];

#pragma acc data copyin(c)
  {
    c = 5;
    ca[3] = c;
    caa[3][12] = ca[3] + caa[3][12];

#pragma acc data copyin(ca[2:4])
    {
      c = 6;
      ca[4] = c;
      caa[3][12] = ca[3] + caa[3][12];
    }

#pragma acc parallel copyout(ca[3:4])
    {
      c = 7;
      ca[5] = c;
      caa[3][12] = ca[3] + caa[3][12];
    }

#pragma acc kernels copy(ca[4:4])
    {
      c = 8;
      ca[6] = c;
      caa[3][12] = ca[3] + caa[3][12];
    }

#pragma acc serial copyout(ca[3:4])
    {
      c = 9;
      ca[11] = c;
      caa[3][12] = ca[3] + caa[3][12];
    }

#pragma acc data pcopy(ca[5:7])
    {
      c = 15;
      ca[7] = c;
      caa[3][12] = ca[3] + caa[3][12];

#pragma acc data pcopyin(caa[3:7][0:30])
      {
	c = 16;
	ca[8] = c;
	caa[3][12] = ca[3] + caa[3][12];
      }

#pragma acc parallel pcopyout(caa[3:7][0:30])
      {
	c = 17;
	ca[9] = c;
	caa[3][12] = ca[3] + caa[3][12];
      }

#pragma acc kernels pcopy(caa[3:7][0:30])
      {
	c = 18;
	ca[10] = c;
	caa[3][12] = ca[3] + caa[3][12];
      }

#pragma acc serial pcopyout(caa[3:7][0:30])
      {
	c = 19;
	ca[12] = c;
	caa[3][12] = ca[3] + caa[3][12];
      }
    }
  }
}
