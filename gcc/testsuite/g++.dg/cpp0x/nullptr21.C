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

  nullptr_t mynull = 0;
  try {
    throw mynull;
  } catch (void*) {
    printf("Test 2 Fail");
  } catch (bool) {
    printf("Test 2 Fail");
  } catch (int) {
    printf("Test 2 Fail");
  } catch (long int) {
    printf("Test 2 Fail");
  } catch (nullptr_t) {
    printf("Test 2 OK");
  } catch (...) {
    printf("Test 2 Fail");
  }  // { dg-output "Test 2 OK" }
}
