// { dg-options "" }

int main(void) {
  static const void* lbls[2][2] = {{&&lbl0, &&lbl0}, {&&lbl0, &&lbl0}};
  goto *lbls[0];
 lbl0:
  ;
}
