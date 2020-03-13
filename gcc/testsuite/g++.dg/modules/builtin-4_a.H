// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

void* operator new(__SIZE_TYPE__);
void* operator new[](__SIZE_TYPE__);

void operator delete (void*) noexcept;
void operator delete[](void*) noexcept;

