/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

class ToBeunavailable {
} __attribute__ ((unavailable ("unavailable!")));

typedef ToBeunavailable NotToBeunavailable; // { dg-error "'ToBeunavailable' is unavailable" }

int main() {

  ToBeunavailable();    // { dg-error "'ToBeunavailable' is unavailable" }
  ToBeunavailable x;    // { dg-error "'ToBeunavailable' is unavailable" }

  NotToBeunavailable();
  NotToBeunavailable y;
}
