/* { dg-do compile } */

typedef int pixval;
typedef struct { pixval r, g, b; } xel;
int convertRow_sample, convertRaster_col;
short *convertRow_samplebuf;
xel *convertRow_xelrow;
short convertRow_spp;
void fn1() {
    int *alpharow;
    for (; convertRaster_col;
	 ++convertRaster_col, convertRow_sample += convertRow_spp) {
	convertRow_xelrow[convertRaster_col].r =
	    convertRow_xelrow[convertRaster_col].g =
	    convertRow_xelrow[convertRaster_col].b =
	    convertRow_samplebuf[convertRow_sample];
	alpharow[convertRaster_col] = convertRow_samplebuf[convertRow_sample + 3];
    }
}
