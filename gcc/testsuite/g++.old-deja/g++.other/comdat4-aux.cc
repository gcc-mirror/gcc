extern void
bar (int x);

inline void
foo (int i)
{
  switch (i)
    {
    case 3:
    case 5:
    case 6:
    case 9:
    case 15:
      bar (1);
      break;
    case 2:
    case 4:
    case 7:
    case 10:
    case 11:
    case 12:
      bar (2);
      break;
    case 0:
    case 1:
    case 8:
    case 13:
    case 16:
      bar (3);
      break;
    case 14:
      bar (4);
      break;
    default:
      bar (5);
      break;
    }
}

void *fooaddr2 = (void *) foo;
