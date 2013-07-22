struct BlobSpan {
	int right;
};
/* This test makes sure we don't accidentally cause a bad insertion to occur
   by choosing the wrong variable name so that we end up with a use not
   dominated by a def. */
void render_blob_line(struct BlobSpan blobdata) {
	int buf[4 * 8];
	int *data = buf;
	int i, n = 0;
	if (blobdata.right)
		n++;
	if (n)
		for (; i < 2 * n;)
			data[i] = 0;
	n *= 2;
	for (; n;) ;
}
