/* { dg-do compile } */
/* { dg-options "-O2 -fshort-enums" } */

enum re {
  o3,
};

int
uj (int mq, enum re dn)
{
  enum re nr = mq;

  switch (nr)
    {
    case 4:
      if (dn == 0)
        goto wdev_inactive_unlock;
      break;

    default:
      break;
    }

  switch (nr)
    {
    case 0:
    case 4:
      return 0;

    default:
      break;
    }

 wdev_inactive_unlock:
  return 1;
}
