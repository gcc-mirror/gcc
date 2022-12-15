#include <exception>
#include <contract>
#include <dlfcn.h>

using handler = void (*)(const std::contract_violation &);
constexpr const char *mangledHandlerName = "_Z25handle_contract_violationRKSt18contract_violation";
void handle_contract_violation(const std::contract_violation &violation) {
  try {
    handler original_handle_contract_violation;
    original_handle_contract_violation =
      (handler)dlsym(RTLD_NEXT, mangledHandlerName);
    (*original_handle_contract_violation)(violation);
  }
  catch(...) {
    ; // squash
  }
  std::terminate();
}

