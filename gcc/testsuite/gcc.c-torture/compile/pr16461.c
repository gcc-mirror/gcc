/* PR tree-optimization/16461  */

int DVDinput_read(int);
int DVDReadBlocksPath(int offset, int block_count) {
    int ret = 0, ret2 = 0;
    for (;;) {
	if (offset)
	    ret = DVDinput_read(block_count);
	else 
	    ret2 = DVDinput_read(block_count);
	break;
    }
    return ret + ret2;
}
