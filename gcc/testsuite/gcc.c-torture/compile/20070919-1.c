typedef short int int16_t;
typedef unsigned char uint8_t;
typedef struct {
	int16_t LARc[8];
	int16_t Nc[4];
	int16_t bc[4];
	int16_t Mc[4];
	int16_t xmaxc[4];
	int16_t xMc[4][13];
} gsm0610_frame_t;
int gsm0610_unpack_voip(gsm0610_frame_t * s, const uint8_t c[33])
{
	int i;
	for (i = 0; i < 4; i++) {
		s->Nc[i] = (*c >> 1) & 0x7F;
		s->bc[i] = (*c++ & 0x1) << 1;
		s->bc[i] |= (*c >> 7) & 0x1;
		s->Mc[i] = (*c >> 5) & 0x3;
		s->xmaxc[i] = (*c++ & 0x1F) << 1;
		s->xmaxc[i] |= (*c >> 7) & 0x1;
		s->xMc[i][0] = (*c >> 4) & 0x7;
		s->xMc[i][1] = (*c >> 1) & 0x7;
		s->xMc[i][2] = (*c++ & 0x1) << 2;
		s->xMc[i][2] |= (*c >> 6) & 0x3;
		s->xMc[i][3] = (*c >> 3) & 0x7;
		s->xMc[i][4] = *c++ & 0x7;
		s->xMc[i][10] |= (*c >> 6) & 0x3;
		s->xMc[i][11] = (*c >> 3) & 0x7;
		s->xMc[i][12] = *c++ & 0x7;
	}
}
