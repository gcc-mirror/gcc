/* PR ipa/98057 */
/* { dg-do compile } */                                                                        
/* { dg-options "-O3 -ffunction-sections" } */

class JITSymbolResolver {
  virtual void anchor();
};
class MemoryManager {
  virtual void anchor();
};
class MCJITMemoryManager : MemoryManager {
  void anchor();
};
class RTDyldMemoryManager : MCJITMemoryManager, JITSymbolResolver {
  void anchor();
};
void RTDyldMemoryManager::anchor() {}
void MCJITMemoryManager::anchor() {}
