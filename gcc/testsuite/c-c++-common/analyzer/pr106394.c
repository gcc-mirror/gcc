struct msm_gpu {
  // [...snip...]
  const struct msm_gpu_perfcntr *perfcntrs;
  // [...snip...]
};

struct msm_gpu_perfcntr {
  // [...snip...]
  const char *name;
};

static const struct msm_gpu_perfcntr perfcntrs[] = {};

struct msm_gpu *test(struct msm_gpu *gpu) {
  // [...snip...]
  gpu->perfcntrs = perfcntrs;
  // [...snip...]
  return gpu;
}
