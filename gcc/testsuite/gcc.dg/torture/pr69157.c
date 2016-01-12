/* { dg-do compile } */

typedef struct {
    float *data_normal3f;
    float *data_texcoordtexture2f;
    float *data_texcoordlightmap2f;
    float *data_color4f;
} dp_model_t;
dp_model_t a;
float *b;
void fn1() {
    int c;
    a.data_normal3f = b + c * 3;
    a.data_texcoordtexture2f = a.data_normal3f + c * 3;
    a.data_texcoordlightmap2f = a.data_color4f =
	a.data_texcoordlightmap2f + c * 2;
}
