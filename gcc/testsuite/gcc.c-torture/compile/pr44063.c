/* { dg-skip-if "exceeds eBPF stack limit" { bpf-*-* } } */

typedef signed char int8_t;
typedef short int16_t;
typedef unsigned char uint8_t;
typedef unsigned int uint32_t;

union unaligned_32 {uint32_t l;} __attribute__((packed)) __attribute__((may_alias));
static inline uint32_t NEG_USR32(uint32_t a, int8_t s){return a << (32 - s);}
typedef struct GetBitContext { const uint8_t *buffer, *buffer_end; int index;}GetBitContext;
typedef struct VLC {int16_t (*table)[2];} VLC;
static __attribute__((always_inline)) inline int get_vlc2(GetBitContext *s, int16_t (*table)[2], int bits, int max_depth) {
    unsigned int re_index= (s)->index;
    int re_cache= 0;
    {
        int n, nb_bits;
        unsigned int index;
        index= NEG_USR32(re_cache, bits);
        n = table[index][1];
        if(max_depth > 1 && n < 0){
            re_cache= bswap_32((((const union unaligned_32 *) (((const uint8_t *)(s)->buffer)+(re_index>>3)))->l)) << (re_index&0x07);
        }
    }
}
typedef struct HYuvContext{GetBitContext gb; int decorrelate; int bitstream_bpp; uint8_t *temp[3]; VLC vlc[6];} HYuvContext;
static __attribute__((always_inline)) inline void decode_bgr_1(HYuvContext *s, int count, int decorrelate, int alpha){
    int i;
        int code = get_vlc2(&s->gb, s->vlc[3].table, 11, 1);
        if(code != -1){
            s->temp[0][4*i+0] = get_vlc2(&s->gb, s->vlc[0].table, 11, 3);
            s->temp[0][4*i+1] = get_vlc2(&s->gb, s->vlc[1].table, 11, 3);
            s->temp[0][4*i+2] = get_vlc2(&s->gb, s->vlc[2].table, 11, 3);
        }
}
void decode_bgr_bitstream(HYuvContext *s, int count){
    if(s->decorrelate){
        if(s->bitstream_bpp==24) decode_bgr_1(s, count, 1, 0);
        else             decode_bgr_1(s, count, 1, 1);
    }
}
