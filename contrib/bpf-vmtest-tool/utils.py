import subprocess
import logging
from typing import Any

logger = logging.getLogger(__name__)


def run_command(cmd: list[str], stream_output: bool = False, **kwargs: Any):
    cleaned_cmd = [str(item) for item in cmd if str(item).strip()]
    capture_cmd_output = not stream_output
    try:
        logger.debug(f"running command: {cleaned_cmd}")
        result = subprocess.run(
            cleaned_cmd,
            text=True,
            check=True,
            capture_output=capture_cmd_output,
            shell=False,
            **kwargs,
        )
        if capture_cmd_output:
            logger.debug("Command stdout:\n" + result.stdout.strip())
            if result.stderr:
                logger.debug("Command stderr:\n" + result.stderr.strip())
        return result
    except subprocess.CalledProcessError as e:
        logger.error(e)
        if capture_cmd_output:
            logger.error("Command failed with stdout: %s", e.stdout.strip())
            logger.error("Command failed with stderr: %s", e.stderr.strip())
        raise
