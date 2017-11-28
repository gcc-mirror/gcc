/* { dg-do compile } */
/* { dg-options "-O -ftree-loop-distribution -floop-block -Wno-return-type" } */

typedef struct stSirenEncoder { } *SirenEncoder;

int Siren7_EncodeFrame(SirenEncoder encoder, unsigned char *DataIn, unsigned char *DataOut) {
   int number_of_regions;
   static int absolute_region_power_index[28] = {0};
   static int region_mlt_bit_counts[28] = {0};
   int dwRes = 0;
   int region;
   if (dwRes != 0) return dwRes;
   for(region = 0; region < number_of_regions; region++)
   {
     absolute_region_power_index[region] += 24;
     region_mlt_bit_counts[region] = 0;
   }
}
