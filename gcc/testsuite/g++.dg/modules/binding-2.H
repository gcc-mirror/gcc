// { dg-additional-options "-fmodule-header -Wextra" }
// { dg-module-cmi {} }

void* operator new(__SIZE_TYPE__, void* __p) noexcept;
void* operator new[](__SIZE_TYPE__, void* __p) noexcept;

void operator delete (void*, void*) noexcept;
void operator delete[](void*, void*) noexcept;

