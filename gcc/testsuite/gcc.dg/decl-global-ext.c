int merror = 0;
extern int merror;

void mtherr (int code)       
{
  merror = code + 1;
}

int main()
{
	mtherr(7);
	return 0;
}
