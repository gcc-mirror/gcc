void
init_movntdqa (int *src)
{
  int i, j, sign = 1;

  for (i = 0; i < 20; i++)
    for (j = 0; j < 4; j++)
      {
	src[i * 4 + j] = j * i * i * sign;
	sign = -sign;
      }
}

