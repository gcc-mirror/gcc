/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File - Unsafe_crc16.i

long BGl_crczd2valuezd2zz__crc16z00(long BgL_valz00_4, long BgL_crcz00_5)
{
  {
    {
      long BgL_iz00_1796;
      long BgL_valuez00_1797;
      long BgL_crcz00_1798;

      BgL_iz00_1796 = ((long) 0);
      BgL_valuez00_1797 = (BgL_valz00_4 << (int) (((long) 8)));
      BgL_crcz00_1798 = BgL_crcz00_5;
      BgL_loopz00_1795:
      if ((BgL_iz00_1796 == ((long) 8)))
	{
	  return BgL_crcz00_1798;
	}
      else
	{
	  long BgL_valuez00_1801;
	  long BgL_crcz00_1802;

	  BgL_valuez00_1801 = (BgL_valuez00_1797 << (int) (((long) 1)));
	  BgL_crcz00_1802 = (BgL_crcz00_1798 << (int) (((long) 1)));
	  {
	    long BgL_crcz00_2209;
	    long BgL_valuez00_2208;
	    long BgL_iz00_2206;

	    BgL_iz00_2206 = (BgL_iz00_1796 + ((long) 1));
	    BgL_valuez00_2208 = BgL_valuez00_1801;
	    if (
		(((long) 0) ==
		 (((long) 65536) & (BgL_crcz00_1802 ^ BgL_valuez00_1801))))
	      {
		BgL_crcz00_2209 = BgL_crcz00_1802;
	      }
	    else
	      {
		BgL_crcz00_2209 = (BgL_crcz00_1802 ^ ((long) 32773));
	      }
	    BgL_crcz00_1798 = BgL_crcz00_2209;
	    BgL_valuez00_1797 = BgL_valuez00_2208;
	    BgL_iz00_1796 = BgL_iz00_2206;
	    goto BgL_loopz00_1795;
	  }
	}
    }
  }
}

/* { dg-final { scan-tree-dump "Polynomial's value is \\\{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0\\\}
" "crc" } } */
/* { dg-final { scan-tree-dump "maybe contains CRC calculation." "crc" } } */
