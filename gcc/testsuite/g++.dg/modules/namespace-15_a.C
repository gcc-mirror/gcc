// Test that namespace deprecation is represented in the gcm.

// { dg-additional-options "-fmodules" }

export module M;

export {
  namespace [[deprecated]] N { }
}
