void gl_fog_index_pixels(float f, unsigned int n, unsigned int index[])
{ 
  unsigned int i;
  for (i=0; i<n; i++) 
    index[i] = (unsigned int) ((float) index[i] + (1.0F-f));
}

