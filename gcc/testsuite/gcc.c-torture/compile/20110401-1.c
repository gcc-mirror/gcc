void asn1_length_der (unsigned long int len, unsigned char *ans, int *ans_len)
{
    int k;
    unsigned char temp[4];
    if (len < 128) {
	if (ans != ((void *) 0))
	    ans[0] = (unsigned char) len;
	*ans_len = 1;
    } else {
	k = 0;
	while (len) {
	    temp[k++] = len & 0xFF;
	    len = len >> 8;
	}
	*ans_len = k + 1;
	if (ans != ((void *) 0)) {
	    ans[0] = ((unsigned char) k & 0x7F) + 128;
	    while (k--)
		ans[*ans_len - 1 - k] = temp[k];
	}
    }
}
