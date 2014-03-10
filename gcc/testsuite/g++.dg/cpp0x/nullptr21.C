// { dg-do run { target c++11 } }

// Test throw and catch

extern "C" void abort (void);

typedef decltype(nullptr) nullptr_t;

int result[2];

void __attribute__((noinline))
foo (int i, int j)
{
  result[i] = j;
}

int main()
{
  try {
    throw nullptr;
  } catch (void*) {
    foo (0, 1);
  } catch (bool) {
    foo (0, 2);
  } catch (int) {
    foo (0, 3);
  } catch (long int) {
    foo (0, 4);
  } catch (nullptr_t) {
    foo (0, 5);
  } catch (...) {
    foo (0, 6);
  }

  nullptr_t mynull = 0;
  try {
    throw mynull;
  } catch (void*) {
    foo (1, 1);
  } catch (bool) {
    foo (1, 2);
  } catch (int) {
    foo (1, 3);
  } catch (long int) {
    foo (1, 4);
  } catch (nullptr_t) {
    foo (1, 5);
  } catch (...) {
    foo (1, 6);
  }

  if (result[0] != 5 || result[1] != 5)
    abort ();
}
