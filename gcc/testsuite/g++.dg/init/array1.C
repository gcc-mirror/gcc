// Test that completing an array declared with a typedef doesn't change
// the typedef.

// { dg-do run }

typedef int iArr[];

const iArr array4={
  1, 2, 3, 4
};

const iArr array3={
  1, 2, 3
};

const iArr array5={
  1, 2, 3, 4, 5
};

int main()
{
  if (sizeof (array4)/sizeof (array4[0]) != 4
      || sizeof (array3)/sizeof (array3[0]) != 3
      || sizeof (array5)/sizeof (array5[0]) != 5)
    return 1;
}
