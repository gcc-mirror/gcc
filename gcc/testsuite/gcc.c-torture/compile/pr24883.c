typedef struct _rec_stl rec_stl;
struct _rec_stl {
   unsigned char **strs;
};
orec_str_list(int count) {
   rec_stl *stl;
   int i, j;
   int li, lj;
   unsigned char ci, cj;
   for (i = 0; i < count; i++) {
      for (j = i + 1; j < count; j++) {
         cj = lj > 2 ? stl->strs[j][0] : (long)stl->strs[j] & 0xff;
         if ((count >= 16 && cj < ci) || (cj == ci && lj > li)) {
            stl->strs[j] = stl->strs[i];
            ci ^= cj;
            cj ^= ci;
            ci ^= cj;
         }
      }
   }
}
