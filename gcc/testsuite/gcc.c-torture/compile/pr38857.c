static const int vs_total_ac_bits = 2680;
typedef struct EncBlockInfo {
      short mb[64];
      unsigned char next[64];
} EncBlockInfo;
inline void dv_guess_qnos(EncBlockInfo* blks, int* qnos) {
      int size[5];
      int j, k, a, prev;
      EncBlockInfo* b;
      for(a=2; a==2 || vs_total_ac_bits < size[0]; a+=a){
	 for (j=0; j<6*5; j++, b++) {
	     for (k= b->next[prev]; k<64; k= b->next[k]) {
		 if(b->mb[k] < a && b->mb[k] > -a){
		     b->next[prev] = b->next[k];
		 }
		 else{
		     prev = k;
		 }
	     }
	 }
     }
}
