// { dg-do compile }
/* { dg-additional-options "-fno-strict-aliasing" } */
enum VkStructureType {
  VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT,
  VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR
} typedef VkPhysicalDeviceSparseProperties;
struct VkPhysicalDeviceProperties {
  int apiVersion;
  VkPhysicalDeviceSparseProperties sparseProperties;
};
typedef struct {
  VkStructureType sType;
  int *pPhysicalDevices;
} VkPhysicalDeviceFeatures2;
typedef struct VkPhysicalDeviceProperties2 {
  VkStructureType sType;
  void *pNext;
} VkPhysicalDeviceMemoryProperties2;
struct VulkanVersion {
  int major;
  int minor;
  int patch;
};
int make_vulkan_version_version;
VulkanVersion make_vulkan_version() {
  return {make_vulkan_version_version, make_vulkan_version_version,
          make_vulkan_version_version};
}
struct AppGpu {
  int &inst;
  int id;
  int *phys_device = nullptr;
  VulkanVersion api_version{};
  VkPhysicalDeviceProperties props{};
  VkPhysicalDeviceProperties2 props2{};
  int memory_props{};
  VkPhysicalDeviceMemoryProperties2 memory_props2{};
  int features{};
  VkPhysicalDeviceFeatures2 features2{};
  int *dev = nullptr;
  int enabled_features{};
  int AppGpu_phys_device;
  int AppGpu_inst;
  AppGpu() : inst(AppGpu_inst), id() {
    api_version = make_vulkan_version();
    props2.sType = memory_props2.sType = features2.sType =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR;
  }
};
int
main() { AppGpu(); return 0; }
