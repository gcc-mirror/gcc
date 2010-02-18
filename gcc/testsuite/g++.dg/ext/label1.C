// { dg-options "" }

int main(void) {
  static const void* lbls[2][2] = {{&&lbl0, &&lbl0}, {&&lbl0, &&lbl0}};
  goto *lbls[0][0];
  goto *lbls[0][0][0];		// { dg-message "" }
  goto *lbls[0];		// { dg-error "" }
 lbl0:
  ;
}
