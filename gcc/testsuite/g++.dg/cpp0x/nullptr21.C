// { dg-do run }
// { dg-options "-std=c++0x" }

// Test throw and catch

#include <cstdio>

typedef decltype(nullptr) nullptr_t;

int main()
{
  try {
    throw nullptr;
  } catch (void*) {
    printf("Test 1 Fail");
  } catch (bool) {
    printf("Test 1 Fail");
  } catch (int) {
    printf("Test 1 Fail");
  } catch (long int) {
    printf("Test 1 Fail");
  } catch (nullptr_t) {
    printf("Test 1 OK");
  } catch (...) {
    printf("Test 1 Fail");
  }  // { dg-output "Test 1 OK" }
}
