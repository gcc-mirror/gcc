extern __thread unsigned gd;

unsigned get() {
	return gd;
}
