void f(unsigned char *src, unsigned char *dst, int num, unsigned char *pos, unsigned char *diffuse, int hasdiffuse, unsigned char *specular, int hasspecular) {
    int i;

    for(i=num;i--;) {
	float *p = (float *) ((__SIZE_TYPE__) dst + (__SIZE_TYPE__) pos);
        if(hasdiffuse) {
            unsigned int *dstColor = (unsigned int *) (dst + i + (__SIZE_TYPE__) diffuse);
            *dstColor = * (unsigned int *) ( ((__SIZE_TYPE__) src + (__SIZE_TYPE__) diffuse) + i);
        }
        if(hasspecular) {
            unsigned int *dstColor = (unsigned int *) (dst + i + (__SIZE_TYPE__) specular);
            *dstColor = * (unsigned int *) ( ((__SIZE_TYPE__) src + (__SIZE_TYPE__) specular) + i);
        }
    }
}

