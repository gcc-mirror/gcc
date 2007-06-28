/* { dg-require-visibility "" } */
/* { dg-options "-fvisibility-ms-compat" } */

/* { dg-final { scan-not-hidden "__ZTI1S" } } */
/* { dg-final { scan-hidden "__ZTI1T" } } */
/* { dg-final { scan-not-hidden "__ZTI1U" } } */
/* { dg-final { scan-not-hidden "__ZN1U6hide_4Ev" } } */

class S {
  virtual void hide_2();
} hide_1;

void S::hide_2() {
}

class __attribute__((visibility("hidden"))) T {
  virtual void hide_4();
} hide_3;

void T::hide_4() {
}

class __attribute__((visibility("default"))) U {
  virtual void hide_4();
};

void U::hide_4() {
}
